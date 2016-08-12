#[derive(Debug)]
pub enum Expr {
    EVar(Name),
    ENum(i64),
    EAp(Box<Expr>, Box<Expr>),
    ELet(Vec<LetEq>, Box<Expr>),
    ELetrec(Vec<LetEq>, Box<Expr>),
    ECase(Box<Expr>, Vec<Alter>),
    EConstr(i64, i64),
    ELam(Vec<Name>, Box<Expr>),
}

pub type Name = String;
pub type Program = Vec<ScDef>;
pub type LetEq = (Name, Expr);
pub type Alter = (i64, Vec<Name>, Expr);

#[derive(Debug)]
pub struct ScDef {
    pub name: Name,
    pub args: Vec<Name>,
    pub body: Expr,
}
