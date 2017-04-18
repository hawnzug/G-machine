use ast::{Name, Program, ScDef, Expr, LetEq};
use heap::{Heap, Addr};

use std::collections::HashMap;
use std::mem;

#[derive(Debug)]
pub struct GmState {
    output: GmOutput,
    code: GmCode,
    stack: GmStack,
    dump: GmDump,
    heap: GmHeap,
    globals: GmGlobals,
    stats: GmStats,
}

type GmOutput = String;
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
    Equal,
    Noteq,
    Lt,
    Le,
    Gt,
    Ge,
    Pack(usize, usize),
    Casejump(HashMap<usize, GmCode>),
    Split(usize),
    Print,
}

#[derive(Debug, Clone)]
enum Node {
    Num(i64),
    Ap(Addr, Addr),
    Global(usize, GmCode),
    Ind(Addr),
    Constr(usize, Vec<Addr>),
}

#[derive(Debug, Clone)]
enum Context {
    Strict,
    Lazy,
}

pub fn compile(prog: Program) -> GmState {
    use self::Instruction::{Eval, PushGlobal, Print};
    let init_code = vec![Print, Eval, PushGlobal(String::from("main"))];
    let (heap, globals) = build_initial_heap(prog);
    GmState {
        output: String::new(),
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
             (String::from("=="),
              2,
              vec![Unwind, Pop(2), Update(2), Equal, Eval, Push(1), Eval, Push(1)]),
             (String::from("!="),
              2,
              vec![Unwind, Pop(2), Update(2), Noteq, Eval, Push(1), Eval, Push(1)]),
             (String::from("<"),
              2,
              vec![Unwind, Pop(2), Update(2), Lt, Eval, Push(1), Eval, Push(1)]),
             (String::from("<="),
              2,
              vec![Unwind, Pop(2), Update(2), Le, Eval, Push(1), Eval, Push(1)]),
             (String::from(">"),
              2,
              vec![Unwind, Pop(2), Update(2), Gt, Eval, Push(1), Eval, Push(1)]),
             (String::from(">="),
              2,
              vec![Unwind, Pop(2), Update(2), Ge, Eval, Push(1), Eval, Push(1)]),
             (String::from("negate"), 1, vec![Unwind, Pop(1), Update(1), Neg, Eval, Push(0)])];
    for (name, nargs, code) in primitives {
        let addr = heap.alloc(Node::Global(nargs, code));
        globals.insert(name, addr);
    }
    for scdef in prog {
        let (name, nargs, code) = compile_sc(scdef);
        let addr = heap.alloc(Node::Global(nargs, code));
        globals.insert(name, addr);
    }
    (heap, globals)
}

type GmEnvironment = HashMap<Name, usize>;

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
    let mut compiled = compile_e(expr, env);
    init.append(&mut compiled);
    init
}

enum Flat {
    Pack(usize, usize, Vec<Expr>),
    Other(Expr),
}

fn flatten(mut expr: Expr) -> Flat {
    enum Temp {
        Cons,
        Other,
    }
    let mut flag = Temp::Other;
    {
        let mut count: usize = 0;
        let mut e = &expr;
        while let &Expr::EAp(ref e1, _) = e {
            count += 1;
            e = e1;
        }
        match e {
            &Expr::EConstr(_, a) => {
                if count == a {
                    flag = Temp::Cons;
                }
            }
            _ => {}
        }
    }
    match flag {
        Temp::Other => Flat::Other(expr),
        Temp::Cons => {
            let mut v = Vec::new();
            while let Expr::EAp(e1, e2) = expr {
                v.push(*e2);
                expr = *e1;
            }
            if let Expr::EConstr(t, a) = expr {
                Flat::Pack(t, a, v)
            } else {
                unreachable!()
            }
        }
    }
}

fn compile_e(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::*;
    let expr = match flatten(expr) {
        Flat::Other(expr) => expr,
        Flat::Pack(t, a, v) => {
            let mut init = vec![Pack(t, a)];
            for (n, e) in v.into_iter().enumerate().rev() {
                let new_env = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                init.append(&mut compile_c(e, new_env));
            }
            return init;
        }
    };
    match expr {
        Expr::ENum(i) => vec![Pushint(i)],
        Expr::ELet(defs, box_expr) => compile_let(defs, *box_expr, env, Context::Strict),
        Expr::ELetrec(defs, box_expr) => compile_letrec(defs, *box_expr, env, Context::Strict),
        Expr::ECase(e, alts) => {
            let mut hm = HashMap::new();
            for (t, names, body) in alts {
                let n = names.len();
                let mut code = vec![Slide(n)];
                let mut new_env: GmEnvironment = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                for (i, name) in names.into_iter().enumerate() {
                    new_env.insert(name, i);
                }
                code.append(&mut compile_e(body, new_env));
                code.push(Split(n));
                hm.insert(t, code);
            }
            let mut init = vec![Casejump(hm)];
            init.append(&mut compile_e(*e, env));
            init
        }
        _ => {
            let expr = if let Expr::EAp(e1, e2) = expr {
                let temp = *e1;
                let e1 = match temp {
                    Expr::EVar(name) => {
                        if name == "negate" {
                            let mut init = vec![Neg];
                            let mut compiled = compile_e(*e2, env);
                            init.append(&mut compiled);
                            return init;
                        } else {
                            Box::new(Expr::EVar(name))
                        }
                    }
                    Expr::EAp(e3, e4) => {
                        let e3 = match *e3 {
                            Expr::EVar(name) => {
                                let mut init = match name.as_ref() {
                                    "+" => vec![Add],
                                    "-" => vec![Sub],
                                    "*" => vec![Mul],
                                    "/" => vec![Div],
                                    "==" => vec![Equal],
                                    "!=" => vec![Noteq],
                                    "<=" => vec![Le],
                                    ">=" => vec![Ge],
                                    "<" => vec![Lt],
                                    ">" => vec![Gt],
                                    _ => vec![],
                                };
                                if init.is_empty() {
                                    Box::new(Expr::EVar(name))
                                } else {
                                    let new_env = env.clone()
                                        .into_iter()
                                        .map(|(name, i)| (name, i + 1))
                                        .collect();
                                    let mut compiled = compile_e(*e4, new_env);
                                    init.append(&mut compiled);
                                    let mut compiled = compile_e(*e2, env);
                                    init.append(&mut compiled);
                                    return init;
                                }
                            }
                            _ => e3,
                        };
                        Box::new(Expr::EAp(e3, e4))
                    }
                    _ => Box::new(temp),
                };
                Expr::EAp(e1, e2)
            } else {
                expr
            };
            let mut code = vec![Eval];
            let mut left = compile_c(expr, env);
            code.append(&mut left);
            code
        }
    }
}

fn compile_c(expr: Expr, env: GmEnvironment) -> GmCode {
    use self::Instruction::{Push, PushGlobal, Pushint, Mkap, Pack};
    let expr = match flatten(expr) {
        Flat::Other(expr) => expr,
        Flat::Pack(t, a, v) => {
            let mut init = vec![Pack(t, a)];
            for (n, e) in v.into_iter().enumerate().rev() {
                let new_env = env.clone()
                    .into_iter()
                    .map(|(name, i)| (name, i + n))
                    .collect();
                init.append(&mut compile_c(e, new_env));
            }
            return init;
        }
    };
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
            let new_env = env.clone()
                .into_iter()
                .map(|(name, i)| (name, i + 1))
                .collect();
            let mut compiled1 = compile_c(*box_e1, new_env);
            let mut compiled2 = compile_c(*box_e2, env);
            let mut init = vec![Mkap];
            init.append(&mut compiled1);
            init.append(&mut compiled2);
            init
        }
        Expr::ELet(defs, box_expr) => compile_let(defs, *box_expr, env, Context::Lazy),
        Expr::ELetrec(defs, box_expr) => compile_letrec(defs, *box_expr, env, Context::Lazy),
        _ => unreachable!(),
    }
}

fn compile_let(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment, context: Context) -> GmCode {
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
    let mut code_expr = match context {
        Context::Strict => compile_e(expr, new_env),
        Context::Lazy => compile_c(expr, new_env),
    };
    code.append(&mut code_expr);
    for (n, (_, e)) in defs.into_iter().enumerate().rev() {
        let new_env = env.clone()
            .into_iter()
            .map(|(name, i)| (name, i + n))
            .collect();
        code.append(&mut compile_c(e, new_env));
    }
    code
}

fn compile_letrec(defs: Vec<LetEq>, expr: Expr, env: GmEnvironment, context: Context) -> GmCode {
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
    let mut code_expr = match context {
        Context::Strict => compile_e(expr, new_env),
        Context::Lazy => compile_c(expr, new_env),
    };
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
        println!("{}", self.output);
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
            d @ Add | d @ Sub | d @ Mul | d @ Div | d @ Equal | d @ Noteq | d @ Lt | d @ Le |
            d @ Gt | d @ Ge => self.dyadic(d),
            Neg => self.neg(),
            Pack(t, n) => self.pack(t, n),
            Casejump(hm) => self.casejump(hm),
            Split(n) => self.split(n),
            Print => self.print(),
        }
    }

    fn pushglobal(&mut self, name: Name) {
        let addr = self.globals[&name];
        self.stack.push(addr);
    }

    fn pushint(&mut self, n: i64) {
        let addr = self.heap.alloc(Node::Num(n));
        self.stack.push(addr);
    }

    fn mkap(&mut self) {
        let addr1 = self.stack.pop().unwrap();
        let addr2 = self.stack.pop().unwrap();
        let addr = self.heap.alloc(Node::Ap(addr1, addr2));
        self.stack.push(addr);
    }

    fn push(&mut self, n: usize) {
        let addr = self.stack[self.stack.len() - n - 1];
        self.stack.push(addr);
    }

    fn update(&mut self, n: usize) {
        let a = self.stack[self.stack.len() - 1];
        let an = self.stack[self.stack.len() - n - 2];
        self.heap.update(an, Node::Ind(a));
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
            Node::Num(_) |
            Node::Constr(_, _) => {
                if let Some((old_code, old_stack)) = self.dump.pop() {
                    self.code = old_code;
                    self.stack = old_stack;
                    self.stack.push(addr);
                }
            }
            Node::Ind(i) => {
                self.code.push(Unwind);
                self.stack.pop();
                self.stack.push(i);
            }
            Node::Ap(a, _) => {
                self.code.push(Unwind);
                self.stack.push(a);
            }
            Node::Global(n, c) => {
                let len = self.stack.len();
                if len - 1 < n {
                    if let Some((old_code, old_stack)) = self.dump.pop() {
                        self.code = old_code;
                        let addr = self.stack[0];
                        self.stack = old_stack;
                        self.stack.push(addr);
                    }
                } else {
                    self.code = c;
                    for i in 0..n {
                        let j = len - 1 - i;
                        self.stack[j] = get_arg(self.heap.lookup(self.stack[j - 1]).unwrap());
                    }
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
            let addr = self.heap.alloc(Node::Ind(0));
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
        use self::Instruction::*;
        use self::Node::{Num, Constr};
        let addr1 = self.stack.pop().unwrap();
        let addr2 = self.stack.pop().unwrap();
        let n1 = if let Num(n1) = self.heap.lookup(addr1).unwrap() {
            n1
        } else {
            panic!("not a number");
        };
        let n2 = if let Num(n2) = self.heap.lookup(addr2).unwrap() {
            n2
        } else {
            panic!("not a number");
        };
        let result = match instr {
            Add => Num(n1 + n2),
            Sub => Num(n1 - n2),
            Mul => Num(n1 * n2),
            Div => Num(n1 / n2),
            _ => {
                Constr(if match instr {
                              Equal => n1 == n2,
                              Noteq => n1 != n2,
                              Lt => n1 < n2,
                              Le => n1 <= n2,
                              Gt => n1 > n2,
                              Ge => n1 >= n2,
                              _ => unreachable!(),
                          } {
                           1
                       } else {
                           0
                       },
                       vec![])
            }
        };
        let addr = self.heap.alloc(result);
        self.stack.push(addr);
    }

    fn neg(&mut self) {
        let addr = self.stack.pop().unwrap();
        let n = if let Node::Num(n) = self.heap.lookup(addr).unwrap() {
            n
        } else {
            panic!("not a number");
        };
        let addr = self.heap.alloc(Node::Num(-n));
        self.stack.push(addr);
    }

    fn pack(&mut self, t: usize, n: usize) {
        if self.stack.len() < n {
            panic!("Constructor has too few arguments");
        }
        let len = self.stack.len();
        let addr = self.heap
            .alloc(Node::Constr(t, self.stack.drain(len - n..).rev().collect()));
        self.stack.push(addr);
    }

    fn casejump(&mut self, mut hm: HashMap<usize, GmCode>) {
        let addr = self.stack.last().unwrap();
        if let Node::Constr(t, _) = self.heap.lookup(*addr).unwrap() {
            let mut code = hm.remove(&t).unwrap();
            self.code.append(&mut code);
        } else {
            panic!("Expect a constructor");
        }
    }

    fn split(&mut self, n: usize) {
        let addr = self.stack.pop().unwrap();
        if let Node::Constr(_, ss) = self.heap.lookup(addr).unwrap() {
            if ss.len() != n {
                panic!("Split: n != constructor's arity")
            }
            self.stack.extend(ss.iter().rev());
        } else {
            panic!("Split: Expect a constructor");
        }
    }

    fn print(&mut self) {
        use self::Instruction::{Eval, Print};
        let addr = self.stack.pop().unwrap();
        match self.heap.lookup(addr).unwrap() {
            Node::Num(n) => self.output += &n.to_string(),
            Node::Constr(_, ss) => {
                let n = ss.len();
                self.stack.extend(ss.iter().rev());
                for _ in 0..n {
                    self.code.push(Print);
                    self.code.push(Eval);
                }
            }
            _ => panic!("Print error"),
        }
    }
}

fn get_arg(node: Node) -> Addr {
    match node {
        Node::Ap(_, addr) => addr,
        _ => unreachable!(),
    }
}
