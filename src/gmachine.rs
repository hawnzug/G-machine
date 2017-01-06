use ast::{Name, Program, ScDef, Expr, LetEq};
use heap::{Heap, Addr};

use std::collections::HashMap;
use std::mem;

#[derive(Debug)]
pub struct GmState {
    code: GmCode,
    stack: GmStack,
    dump: GmDump,
    heap: GmHeap,
    globals: GmGlobals,
    stats: GmStats,
}

type GmCode = Vec<Instruction>;
type GmStack = Vec<Addr>;
type GmDump = Vec<GmDumpItem>;
type GmDumpItem = (GmCode, GmStack);
type GmHeap = Heap<Node>;
type GmGlobals = HashMap<Name, Addr>;
type GmStats = u32;

#[derive(Debug, PartialEq, Clone)]
enum Instruction {
    Unwind,
    PushGlobal(Name),
    Pushint(i64),
    Push(usize),
    Mkap,
    Update(usize),
    Pop(usize),
    Slide(usize),
    Alloc(usize),
    Eval,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
}

#[derive(Debug, Clone)]
enum Node {
    NNum(i64),
    NAp(Addr, Addr),
    NGlobal(usize, GmCode),
    NInd(Addr),
}

pub fn compile(prog: Program) -> GmState {
    use self::Instruction::{Eval, PushGlobal};
    let init_code = vec![Eval, PushGlobal(String::from("main"))];
    let (heap, globals) = build_initial_heap(prog);
    GmState {
        code: init_code,
        stack: Vec::new(),
        dump: Vec::new(),
        heap: heap,
        globals: globals,
        stats: 0,
    }
}

fn build_initial_heap(prog: Program) -> (GmHeap, GmGlobals) {
    use self::Instruction::*;
    let mut heap = Heap::new();
    let mut globals = HashMap::new();
    let primitives =
        vec![(String::from("+"),
              2,
              vec![Unwind, Pop(2), Update(2), Add, Eval, Push(1), Eval, Push(1)]),
             (String::from("-"),
              2,
              vec![Unwind, Pop(2), Update(2), Sub, Eval, Push(1), Eval, Push(1)]),
             (String::from("*"),
              2,
              vec![Unwind, Pop(2), Update(2), Mul, Eval, Push(1), Eval, Push(1)]),
             (String::from("/"),
              2,
              vec![Unwind, Pop(2), Update(2), Div, Eval, Push(1), Eval, Push(1)]),
             (String::from("negate"), 1, vec![Unwind, Pop(1), Update(1), Neg, Eval, Push(0)])];
    for (name, nargs, code) in primitives {
        let addr = heap.alloc(Node::NGlobal(nargs, code));
        globals.insert(name, addr);
    }
    for scdef in prog {
        let (name, nargs, code) = compile_sc(scdef);
        let addr = heap.alloc(Node::NGlobal(nargs, code));
        globals.insert(name, addr);
    }
    (heap, globals)
}

type GmEnvironment = HashMap<Name, Addr>;

fn compile_sc(scdef: ScDef) -> (Name, usize, GmCode) {
    let len = scdef.args.len();
    let mut env: GmEnvironment = HashMap::new();
    for (i, arg) in scdef.args.into_iter().enumerate() {
        env.insert(arg, i);
    }
    (scdef.name, len, compile_r(scdef.body, env))
}

fn compile_r(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Unwind, Pop, Update};
    let mut init = vec![Unwind, Pop(env.len()), Update(env.len())];
    let mut compiled = compile_c(expr, env);
    init.append(&mut compiled);
    init
}

fn compile_c(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Push, PushGlobal, Pushint, Mkap};
    match expr {
        Expr::EVar(name) => {
            if env.contains_key(&name) {
                vec![Push(env[&name])]
            } else {
                vec![PushGlobal(name)]
            }
        }
        Expr::ENum(n) => vec![Pushint(n)],
        Expr::EAp(box_e1, box_e2) => {
            let new_env = env.clone().into_iter().map(|(name, i)| (name, i + 1)).collect();
            let mut compiled1 = compile_c(*box_e1, new_env);
            let mut compiled2 = compile_c(*box_e2, env);
            let mut init = vec![Mkap];
            init.append(&mut compiled1);
            init.append(&mut compiled2);
            init
        }
        Expr::ELet(defs, box_expr) => compile_let(defs, *box_expr, env),
        Expr::ELetrec(defs, box_expr) => compile_letrec(defs, *box_expr, env),
        _ => unreachable!(),
    }
}

fn compile_let(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::Slide;
    let len = defs.len();
    let mut code = vec![Slide(len)];
    let mut new_env: GmEnvironment = env.clone()
        .into_iter()
        .map(|(name, i)| (name, i + len))
        .collect();
    for (i, pair) in defs.iter().enumerate() {
        new_env.insert(pair.0.clone(), len - i - 1);
    }
    let mut code_expr = compile_c(expr, new_env);
    code.append(&mut code_expr);
    for (n, (_, e)) in defs.into_iter().enumerate().rev() {
        let new_env = env.clone().into_iter().map(|(name, i)| (name, i + n)).collect();
        code.append(&mut compile_c(e, new_env));
    }
    code
}

fn compile_letrec(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Slide, Update, Alloc};
    let len = defs.len();
    let mut code = vec![Slide(len)];
    let mut new_env: GmEnvironment = env.into_iter()
        .map(|(name, i)| (name, i + len))
        .collect();
    for (i, pair) in defs.iter().enumerate() {
        new_env.insert(pair.0.clone(), len - i - 1);
    }
    let new_env_clone = new_env.clone();
    let mut code_expr = compile_c(expr, new_env);
    code.append(&mut code_expr);
    for (n, (_, e)) in defs.into_iter().enumerate().rev() {
        let new_env = new_env_clone.clone();
        code.push(Update(n));
        code.append(&mut compile_c(e, new_env));
    }
    code.push(Alloc(len));
    code
}

fn is_final(state: &GmState) -> bool {
    state.code.is_empty()
}

pub fn eval(mut state: GmState) -> GmState {
    while !is_final(&state) {
        state.step();
    }
    state
}

impl GmState {
    pub fn step(&mut self) {
        self.stats += 1;
        let instr = self.code.pop().unwrap();
        self.dispatch(instr);
    }

    pub fn show(&self) {
        println!("{:#?}", self.heap.lookup(self.stack[0]).unwrap());
    }

    fn dispatch(&mut self, instr: Instruction) {
        use self::Instruction::*;
        match instr {
            PushGlobal(f) => self.pushglobal(f),
            Pushint(n) => self.pushint(n),
            Mkap => self.mkap(),
            Push(n) => self.push(n),
            Update(n) => self.update(n),
            Pop(n) => self.pop(n),
            Unwind => self.unwind(),
            Slide(n) => self.slide(n),
            Alloc(n) => self.alloc(n),
            Eval => self.eval(),
            d @ Add | d @ Sub | d @ Mul | d @ Div => self.dyadic(d),
            Neg => self.neg(),
        }
    }

    fn pushglobal(&mut self, name: Name) {
        let addr = self.globals[&name];
        self.stack.push(addr);
    }

    fn pushint(&mut self, n: i64) {
        let addr = self.heap.alloc(Node::NNum(n));
        self.stack.push(addr);
    }

    fn mkap(&mut self) {
        let addr1 = self.stack.pop().unwrap();
        let addr2 = self.stack.pop().unwrap();
        let addr = self.heap.alloc(Node::NAp(addr1, addr2));
        self.stack.push(addr);
    }

    fn push(&mut self, n: usize) {
        let addr = self.stack[self.stack.len() - n - 1];
        self.stack.push(addr);
    }

    fn update(&mut self, n: usize) {
        let a = self.stack[self.stack.len() - 1];
        let an = self.stack[self.stack.len() - n - 2];
        self.heap.update(an, Node::NInd(a));
        self.stack.pop();
    }

    fn pop(&mut self, n: usize) {
        let i = self.stack.len() - n;
        self.stack.truncate(i);
    }

    fn unwind(&mut self) {
        use self::Instruction::Unwind;
        let addr = self.stack[self.stack.len() - 1];
        match self.heap.lookup(addr).unwrap() {
            Node::NNum(_) => {
                if let Some((old_code, old_stack)) = self.dump.pop() {
                    self.code = old_code;
                    self.stack = old_stack;
                    self.stack.push(addr);
                }
            }
            Node::NInd(i) => {
                self.code.push(Unwind);
                self.stack.pop();
                self.stack.push(i);
            }
            Node::NAp(a, _) => {
                self.code.push(Unwind);
                self.stack.push(a);
            }
            Node::NGlobal(n, c) => {
                self.code = c;
                let len = self.stack.len();
                for i in 0..n {
                    let j = len - 1 - i;
                    self.stack[j] = get_arg(self.heap.lookup(self.stack[j - 1]).unwrap());
                }
            }
        }
    }

    fn slide(&mut self, n: usize) {
        let len = self.stack.len();
        let top = self.stack[len - 1];
        self.stack.truncate(len - n - 1);
        self.stack.push(top);
    }

    fn alloc(&mut self, n: usize) {
        for _ in 0..n {
            let addr = self.heap.alloc(Node::NInd(0));
            self.stack.push(addr);
        }
    }

    fn eval(&mut self) {
        use self::Instruction::Unwind;
        let old_code = mem::replace(&mut self.code, vec![Unwind]);
        let addr = self.stack.pop().unwrap();
        let old_stack = mem::replace(&mut self.stack, vec![addr]);
        self.dump.push((old_code, old_stack));
    }

    fn dyadic(&mut self, instr: Instruction) {
        use self::Instruction::{Add, Sub, Mul, Div};
        use self::Node::NNum;
        let addr1 = self.stack.pop().unwrap();
        let addr2 = self.stack.pop().unwrap();
        let n1 = if let NNum(n1) = self.heap.lookup(addr1).unwrap() {
            n1
        } else {
            panic!("not a number");
        };
        let n2 = if let NNum(n2) = self.heap.lookup(addr2).unwrap() {
            n2
        } else {
            panic!("not a number");
        };
        let result = NNum(match instr {
            Add => n1 + n2,
            Sub => n1 - n2,
            Mul => n1 * n2,
            Div => n1 / n2,
            _ => unreachable!(),
        });
        let addr = self.heap.alloc(result);
        self.stack.push(addr);
    }

    fn neg(&mut self) {
        let addr = self.stack.pop().unwrap();
        let n = if let Node::NNum(n) = self.heap.lookup(addr).unwrap() {
            n
        } else {
            panic!("not a number");
        };
        let addr = self.heap.alloc(Node::NNum(-n));
        self.stack.push(addr);
    }
}

fn get_arg(node: Node) -> Addr {
    match node {
        Node::NAp(_, addr) => addr,
        _ => unreachable!(),
    }
}
