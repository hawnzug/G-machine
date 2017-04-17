#[derive(Debug)]
pub enum Expr {
    EVar(Name),
    ENum(i64),
    EAp(Box<Expr>, Box<Expr>),
    ELet(Vec<LetEq>, Box<Expr>),
    ELetrec(Vec<LetEq>, Box<Expr>),
    ECase(Box<Expr>, Vec<Alter>),
    EConstr(usize, usize),
    ELam(Vec<Name>, Box<Expr>),
}

pub type Name = String;
pub type Program = Vec<ScDef>;
pub type LetEq = (Name, Expr);
pub type Alter = (usize, Vec<Name>, Expr);

#[derive(Debug)]
pub struct ScDef {
    pub name: Name,
    pub args: Vec<Name>,
    pub body: Expr,
}
