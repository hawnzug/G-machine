use ast::Name;
use heap::{Heap, Addr};

use std::collections::HashMap;
use std::mem;
use std::fmt;
use std::error;
use std::result;

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
pub type GmHeap = Heap;
pub type GmGlobals = HashMap<Name, Addr>;
pub type GmStats = u32;

pub type Result<T> = result::Result<T, RunTimeError>;

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

    pub fn run(mut self) -> Result<Self> {
        while !self.code.is_empty() {
            self.step()?
        }
        Ok(self)
    }

    pub fn show(&self) {
        println!("{}", self.output);
    }

    fn step(&mut self) -> Result<()> {
        self.stats += 1;
        let instr = self.code.pop().ok_or(RunTimeError::NoInstr)?;
        if self.heap.size() > 200 {
            println!("GC starts. {} nodes", self.heap.size());
            for addr in &self.stack {
                self.heap.mark_rec(*addr);
            }
            for v in &self.dump {
                for addr in &v.1 {
                    self.heap.mark_rec(*addr);
                }
            }
            for (_, addr) in &self.globals {
                self.heap.mark_rec(*addr);
            }
            self.heap.clean();
            println!("GC finishes. {} nodes", self.heap.size());
        }
        self.dispatch(instr)
    }

    fn dispatch(&mut self, instr: Instruction) -> Result<()> {
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

    fn pushglobal(&mut self, name: Name) -> Result<()> {
        use self::RunTimeError::GlobalName;
        let addr = *self.globals.get(&name).ok_or(GlobalName(name))?;
        self.stack.push(addr);
        Ok(())
    }

    fn pushint(&mut self, n: i64) -> Result<()> {
        let addr = self.heap.alloc(Node::Num(n));
        self.stack.push(addr);
        Ok(())
    }

    fn mkap(&mut self) -> Result<()> {
        use self::RunTimeError::EmptyStack;
        let addr1 = self.stack.pop().ok_or(EmptyStack("mkap", 2, 0))?;
        let addr2 = self.stack.pop().ok_or(EmptyStack("mkap", 2, 1))?;
        let addr = self.heap.alloc(Node::Ap(addr1, addr2));
        self.stack.push(addr);
        Ok(())
    }

    fn push(&mut self, n: usize) -> Result<()> {
        use self::RunTimeError::StackIndex;
        let len = self.stack.len();
        let addr = *self.stack
                        .get(len - n - 1)
                        .ok_or(StackIndex("push", len, n + 1))?;
        self.stack.push(addr);
        Ok(())
    }

    fn update(&mut self, n: usize) -> Result<()> {
        use self::RunTimeError::{StackIndex, EmptyStack};
        let a = self.stack.pop().ok_or(EmptyStack("update", 1, 0))?;
        let len = self.stack.len();
        let an = self.stack
            .get(len - n - 1)
            .ok_or(StackIndex("update", len, n + 1))?;
        self.heap.update(*an, Node::Ind(a));
        Ok(())
    }

    fn pop(&mut self, n: usize) -> Result<()> {
        let len = self.stack.len();
        if len < n {
            Err(RunTimeError::EmptyStack("pop", n, len))
        } else {
            self.stack.truncate(len - n);
            Ok(())
        }
    }

    fn unwind(&mut self) -> Result<()> {
        use self::Instruction::Unwind;
        use self::RunTimeError::{StackIndex, HeapAddr, DumpEmpty, EmptyStack, Expect};
        let addr = *self.stack.last().ok_or(StackIndex("unwind", 0, 1))?;
        let node = self.heap.lookup(addr).ok_or(HeapAddr("unwind", addr))?;
        match node {
            Node::Num(_) |
            Node::Constr(_, _) => {
                let (old_code, old_stack) = self.dump.pop().ok_or(DumpEmpty)?;
                self.code = old_code;
                self.stack = old_stack;
                self.stack.push(addr);
            }
            Node::Ind(i) => {
                self.code.push(Unwind);
                self.stack
                    .pop()
                    .ok_or(EmptyStack("unwind(Ind)", 1, 0))?;
                self.stack.push(i);
            }
            Node::Ap(a, _) => {
                self.code.push(Unwind);
                self.stack.push(a);
            }
            Node::Global(n, c) => {
                let len = self.stack.len();
                if len - 1 < n {
                    let (old_code, old_stack) = self.dump.pop().ok_or(DumpEmpty)?;
                    self.code = old_code;
                    let addr = *self.stack
                                    .get(0)
                                    .ok_or(StackIndex("unwind(Global)", 0, 1))?;
                    self.stack = old_stack;
                    self.stack.push(addr);
                } else {
                    self.code = c;
                    for i in 0..n {
                        let j = len - 1 - i;
                        let node = self.heap.lookup(self.stack[j - 1]).ok_or(DumpEmpty)?;
                        let addr = match node {
                            Node::Ap(_, addr) => addr,
                            _ => return Err(Expect("unwind(Global)", "Application")),
                        };
                        self.stack[j] = addr;
                    }
                }
            }
        }
        Ok(())
    }

    fn slide(&mut self, n: usize) -> Result<()> {
        use self::RunTimeError::EmptyStack;
        let len = self.stack.len();
        if len < n + 1 {
            Err(EmptyStack("slide", n + 1, len))
        } else {
            let top = self.stack[len - 1];
            self.stack.truncate(len - n - 1);
            self.stack.push(top);
            Ok(())
        }
    }

    fn alloc(&mut self, n: usize) -> Result<()> {
        for _ in 0..n {
            let addr = self.heap.alloc(Node::Ind(0));
            self.stack.push(addr);
        }
        Ok(())
    }

    fn eval(&mut self) -> Result<()> {
        use self::Instruction::Unwind;
        use self::RunTimeError::EmptyStack;
        let addr = self.stack.pop().ok_or(EmptyStack("eval", 1, 0))?;
        let old_code = mem::replace(&mut self.code, vec![Unwind]);
        let old_stack = mem::replace(&mut self.stack, vec![addr]);
        self.dump.push((old_code, old_stack));
        Ok(())
    }

    fn dyadic(&mut self, instr: Instruction) -> Result<()> {
        use self::Instruction::*;
        use self::Node::{Num, Constr};
        use self::RunTimeError::{EmptyStack, HeapAddr, Expect};
        let n1 = {
            let addr1 = self.stack.pop().ok_or(EmptyStack("dyadic", 2, 0))?;
            let node = self.heap
                .lookup(addr1)
                .ok_or(HeapAddr("dyadic", addr1))?;
            if let Num(n) = node {
                n
            } else {
                return Err(Expect("dyadic", "number"));
            }
        };
        let n2 = {
            let addr2 = self.stack.pop().ok_or(EmptyStack("dyadic", 1, 0))?;
            let node = self.heap
                .lookup(addr2)
                .ok_or(HeapAddr("dyadic", addr2))?;
            if let Num(n) = node {
                n
            } else {
                return Err(Expect("dyadic", "number"));
            }
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
                              _ => return Err(Expect("dyadic", "operator")),
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
        Ok(())
    }

    fn neg(&mut self) -> Result<()> {
        use self::RunTimeError::{EmptyStack, HeapAddr, Expect};
        let addr = self.stack.pop().ok_or(EmptyStack("neg", 1, 0))?;
        let node = self.heap.lookup(addr).ok_or(HeapAddr("neg", addr))?;
        if let Node::Num(n) = node {
            let addr = self.heap.alloc(Node::Num(-n));
            self.stack.push(addr);
            Ok(())
        } else {
            Err(Expect("neg", "number"))
        }
    }

    fn pack(&mut self, t: usize, n: usize) -> Result<()> {
        let len = self.stack.len();
        if len < n {
            Err(RunTimeError::EmptyStack("pack", n, len))
        } else {
            let addr = self.heap
                .alloc(Node::Constr(t, self.stack.drain(len - n..).rev().collect()));
            self.stack.push(addr);
            Ok(())
        }
    }

    fn casejump(&mut self, mut hm: HashMap<usize, GmCode>) -> Result<()> {
        use self::RunTimeError::{EmptyStack, HeapAddr, MissCase, Expect};
        let addr = *self.stack.last().ok_or(EmptyStack("Case", 1, 0))?;
        let node = self.heap.lookup(addr).ok_or(HeapAddr("Case", addr))?;
        if let Node::Constr(t, _) = node {
            let mut code = hm.remove(&t).ok_or(MissCase(t))?;
            self.code.append(&mut code);
            Ok(())
        } else {
            return Err(Expect("casejump", "constructor"));
        }
    }

    fn split(&mut self, n: usize) -> Result<()> {
        use self::RunTimeError::{EmptyStack, ConstrArity, Expect, HeapAddr};
        let addr = self.stack.pop().ok_or(EmptyStack("split", 1, 0))?;
        let node = self.heap.lookup(addr).ok_or(HeapAddr("split", addr))?;
        if let Node::Constr(t, ss) = node {
            if ss.len() != n {
                Err(ConstrArity(t, ss.len(), n))
            } else {
                self.stack.extend(ss.iter().rev());
                Ok(())
            }
        } else {
            Err(Expect("split", "constructor"))
        }
    }

    fn print(&mut self) -> Result<()> {
        use self::Instruction::{Eval, Print};
        use self::RunTimeError::{EmptyStack, HeapAddr, Expect};
        let addr = self.stack.pop().ok_or(EmptyStack("print", 1, 0))?;
        let node = self.heap.lookup(addr).ok_or(HeapAddr("print", addr))?;
        match node {
            Node::Num(n) => {
                self.output += &n.to_string();
                Ok(())
            }
            Node::Constr(_, ss) => {
                let n = ss.len();
                self.stack.extend(ss.iter().rev());
                for _ in 0..n {
                    self.code.push(Print);
                    self.code.push(Eval);
                }
                Ok(())
            }
            _ => Err(Expect("print", "number or constructor")),
        }
    }
}

#[derive(Debug)]
pub enum RunTimeError {
    NoInstr,
    DumpEmpty,
    GlobalName(String),
    EmptyStack(&'static str, usize, usize),
    StackIndex(&'static str, usize, usize),
    HeapAddr(&'static str, Addr),
    MissCase(usize),
    Expect(&'static str, &'static str),
    ConstrArity(usize, usize, usize),
}

impl fmt::Display for RunTimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RunTimeError::*;
        match *self {
            NoInstr => write!(f, "need more instructions"),
            DumpEmpty => write!(f, "Dump is empty"),
            HeapAddr(func, addr) => write!(f, "{}: Heap address {} does not exist", func, addr),
            Expect(func, obj) => write!(f, "{}: Expect a {}", func, obj),
            MissCase(tag) => write!(f, "Miss case for constructor {}", tag),
            GlobalName(ref name) => write!(f, "Cannot find global name {}", name),
            ConstrArity(tag, expect, actual) => {
                write!(f,
                       "Constructor {} expects {}, find {} arguments",
                       tag,
                       expect,
                       actual)
            }
            EmptyStack(func, expect, actual) => {
                write!(f,
                       "{}: Empty stack, expect {}, actual {}",
                       func,
                       expect,
                       actual)
            }
            StackIndex(func, len, index) => {
                write!(f,
                       "{}: Index {} does not exist, stack's length is {}",
                       func,
                       index,
                       len)
            }
        }
    }
}

impl error::Error for RunTimeError {
    fn description(&self) -> &str {
        use self::RunTimeError::*;
        match *self {
            NoInstr => "need more instructions",
            DumpEmpty => "Dump is empty",
            GlobalName(_) => "Missing global name",
            Expect(_, _) => "expect something",
            MissCase(_) => "Missing case",
            EmptyStack(_, _, _) => "Empty Stack",
            StackIndex(_, _, _) => "Stack index does not exist",
            HeapAddr(_, _) => "Heap address does not exist",
            ConstrArity(_, _, _) => "constructor arity mismatch",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            _ => None,
        }
    }
}
