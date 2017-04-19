use gmachine::Node;
use std::mem;

#[derive(Debug)]
pub struct Heap {
    used: Vec<Option<Mark>>,
    free: Vec<Addr>,
}

#[derive(Debug, Clone)]
enum Mark {
    Marked(Node),
    Clean(Node),
}

impl Mark {
    fn inside(self) -> Node {
        match self {
            Mark::Marked(n) => n,
            Mark::Clean(n) => n,
        }
    }
}

pub type Addr = usize;

impl Heap {
    pub fn new() -> Self {
        Heap {
            used: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn lookup(&self, addr: Addr) -> Option<Node> {
        self.used
            .get(addr)
            .cloned()
            .unwrap_or(None)
            .map(|m| m.inside())
    }

    pub fn alloc(&mut self, t: Node) -> Addr {
        if let Some(addr) = self.free.pop() {
            assert!(self.used[addr].is_none());
            self.used[addr] = Some(Mark::Clean(t));
            addr
        } else {
            self.used.push(Some(Mark::Clean(t)));
            self.used.len() - 1
        }
    }

    pub fn update(&mut self, addr: Addr, t: Node) {
        self.used[addr] = Some(Mark::Clean(t));
    }

    #[allow(dead_code)]
    pub fn free(&mut self, addr: Addr) {
        self.used[addr] = None;
        self.free.push(addr);
    }

    #[allow(dead_code)]
    pub fn size(&self) -> usize {
        self.used.len() - self.free.len()
    }

    pub fn mark_rec(&mut self, addr: Addr) {
        if let Some(Mark::Clean(_)) = self.used[addr] {
        } else {
            return;
        }
        let optmk = mem::replace(&mut self.used[addr], None);
        let node = optmk.unwrap().inside();
        match node {
            Node::Ap(addr1, addr2) => {
                self.mark_rec(addr1);
                self.mark_rec(addr2);
            }
            Node::Ind(addr) => {
                self.mark_rec(addr);
            }
            Node::Constr(_, ref addrs) => {
                for addr in addrs {
                    self.mark_rec(*addr);
                }
            }
            _ => {}
        }
        self.used[addr] = Some(Mark::Marked(node));
    }

    pub fn clean(&mut self) {
        for (addr, opt) in self.used.iter_mut().enumerate() {
            if opt.is_none() {
                continue;
            }
            if let Some(Mark::Marked(node)) = mem::replace(opt, None) {
                *opt = Some(Mark::Clean(node));
            } else {
                self.free.push(addr);
            }
        }
    }
}
