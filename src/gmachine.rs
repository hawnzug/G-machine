use ast::Name;
use ast::Program;
use heap::*;

use std::collections::HashMap;

pub struct GmState {
    code: GmCode,
    stack: GmStack,
    heap: GmHeap,
    globals: GmGlobals,
    stats: GmStats,
}

type GmCode = Vec<Instruction>;
type GmStack = Vec<Addr>;
type GmHeap = Heap<Node>;
type GmGlobals = HashMap<Name, Addr>;
type GmStats = u32;

#[derive(Debug, PartialEq, Clone)]
enum Instruction {
    Unwind,
    PushGlobal(Name),
    Pushint(i64),
    Push(u32),
    Mkap,
    Update(u32),
    Pop(u32),
}

#[derive(Debug, Clone)]
enum Node {
    NNum(i64),
    NAp(Addr, Addr),
    NGlobal(u32, GmCode),
    NInd(Addr),
}

#[allow(unused_variables)]
pub fn compile(prog: Program) -> GmState {
    GmState::new()
}

#[allow(unused_variables)]
pub fn eval(state: GmState) -> GmState {
    GmState::new()
}

impl GmState {
    pub fn new() -> Self {
        GmState {
            code: Vec::new(),
            stack: Vec::new(),
            heap: Heap::new(),
            globals: HashMap::new(),
            stats: 0,
        }
    }
}
