#[derive(Debug)]
pub enum Token {
    Equal,
    OpenParen,
    CloseParen,
    Semi,

    Literal(Lit),
    BinOp(BinOpToken),
    Keyword(Key),
    Ident(String),
}

#[derive(Debug)]
pub enum Lit {
    Integer(i64),
}

#[derive(Debug)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Lt,
    Le,
    Gt,
    Ge,
    EqEq,
}

#[derive(Debug)]
pub enum Key {
    Let,
    Letrec,
    Case,
    In,
    Of,
    Pack,
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug)]
pub struct TokenAndPos {
    pub token: Token,
    pub pos: Pos,
}

impl Pos {
    pub fn new(line: u32, column: u32) -> Self {
        Pos {
            line: line,
            column: column,
        }
    }
}
