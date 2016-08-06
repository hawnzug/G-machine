#[allow(dead_code)]
pub enum Expr {
    EVar(Name),
    ENum(i32),
    EAp(Box<Expr>, Box<Expr>),
}

pub type Name = String;
pub type Program = Vec<ScDef>;

pub struct ScDef {
    pub name: Name,
    pub args: Vec<Name>,
    pub body: Expr,
}
