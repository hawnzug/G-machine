#[derive(Debug)]
pub struct Heap<T> {
    used: Vec<Option<T>>,
    free: Vec<Addr>,
}

pub type Addr = usize;

impl<T: Clone> Heap<T> {
    pub fn new() -> Self {
        Heap {
            used: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn lookup(&self, addr: Addr) -> Option<T> {
        self.used.get(addr).cloned().unwrap_or(None)
    }

    pub fn alloc(&mut self, t: T) -> Addr {
        if let Some(addr) = self.free.pop() {
            assert!(self.used[addr].is_none());
            self.used[addr] = Some(t);
            addr
        } else {
            self.used.push(Some(t));
            self.used.len() - 1
        }
    }

    pub fn update(&mut self, addr: Addr, t: T) {
        self.used[addr] = Some(t);
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
}
