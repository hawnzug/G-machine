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

pub type GmOutput = String;
pub type GmCode = Vec<Instruction>;
pub type GmStack = Vec<Addr>;
pub type GmDump = Vec<GmDumpItem>;
pub type GmDumpItem = (GmCode, GmStack);
pub type GmHeap = Heap<Node>;
pub type GmGlobals = HashMap<Name, Addr>;
pub type GmStats = u32;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
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
pub enum Node {
    Num(i64),
    Ap(Addr, Addr),
    Global(usize, GmCode),
    Ind(Addr),
    Constr(usize, Vec<Addr>),
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
    pub fn new(code: GmCode, heap: GmHeap, globals: GmGlobals) -> GmState {
        GmState {
            output: String::new(),
            code: code,
            stack: Vec::new(),
            dump: Vec::new(),
            heap: heap,
            globals: globals,
            stats: 0,
        }
    }

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
