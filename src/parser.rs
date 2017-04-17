use ast::*;
use lexer;
use token::*;

use std::path::Path;
use std::fmt;
use std::error;
use std::result;
use std::mem;

struct Parser {
    tokens: Vec<TokenAndPos>,
    curr_index: usize,
    curr_token: Option<Token>,
}

pub type Result<T> = result::Result<T, ParseError>;

pub fn parse<P: AsRef<Path>>(file_path: P) -> Result<Program> {
    let mut parser = Parser {
        tokens: lexer::tokenize(file_path)?,
        curr_index: 0,
        curr_token: None,
    };
    // println!("{:?}", parser.tokens);
    if !parser.tokens.is_empty() {
        parser.curr_token = Some(parser.tokens[0].token.clone());
    }
    parser.run()
}

impl Parser {
    fn run(&mut self) -> Result<Program> {
        let mut program = Vec::new();
        while let Ok(scdef) = self.parse_scdef() {
            program.push(scdef);
        }
        Ok(program)
    }

    fn bump(&mut self) {
        self.curr_index += 1;
        if self.curr_index < self.tokens.len() {
            self.curr_token = Some(self.tokens[self.curr_index].token.clone());
        } else {
            self.curr_token = None;
        }
    }

    fn parse_name(&mut self) -> Option<String> {
        match self.curr_token {
            Some(Token::Ident(_)) => {
                if let Some(Token::Ident(s)) = mem::replace(&mut self.curr_token, None) {
                    self.bump();
                    Some(s)
                } else {
                    unreachable!()
                }
            }
            _ => None,
        }
    }

    fn parse_num(&mut self) -> Option<i64> {
        match self.curr_token {
            Some(Token::Literal(Lit::Integer(num))) => {
                self.bump();
                Some(num)
            }
            _ => None,
        }
    }

    fn parse_scdef(&mut self) -> Result<ScDef> {
        let name = if let Some(Expr::EVar(s)) = self.parse_ident() {
            s
        } else {
            return Err(ParseError::PlaceHolder);
        };
        let mut args = Vec::new();
        while let Some(Expr::EVar(s)) = self.parse_ident() {
            args.push(s);
        }
        let body = if self.curr_token == Some(Token::Equal) {
            self.bump();
            self.parse_expr().unwrap()
        } else {
            return Err(ParseError::PlaceHolder);
        };
        if self.curr_token == Some(Token::Semi) {
            self.bump();
            Ok(ScDef {
                   name: name,
                   args: args,
                   body: body,
               })
        } else {
            Err(ParseError::PlaceHolder)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.curr_token {
            Some(Token::Keyword(Key::Let)) => self.parse_let(false),
            Some(Token::Keyword(Key::Letrec)) => self.parse_let(true),
            Some(Token::Keyword(Key::Case)) => self.parse_case(),
            Some(Token::BackSlash) => self.parse_lambda(),
            _ => self.parse_lhs(),
        }
    }

    fn parse_primary(&mut self) -> Result<Option<Expr>> {
        if let Some(ident) = self.parse_ident() {
            return Ok(Some(ident));
        }
        if let Some(number) = self.parse_integer() {
            return Ok(Some(number));
        }
        match self.curr_token {
            Some(Token::Keyword(Key::Pack)) => self.parse_pack().map(Some),
            Some(Token::OpenParen) => self.parse_paren().map(Some),
            _ => Ok(None),
        }
    }

    fn parse_apply(&mut self) -> Result<Expr> {
        let mut aps = if let Some(expr) = self.parse_primary()? {
            expr
        } else {
            return Err(ParseError::PlaceHolder);
        };
        while let Some(expr) = self.parse_primary()? {
            aps = Expr::EAp(Box::new(aps), Box::new(expr));
        }
        Ok(aps)
    }

    fn parse_ident(&mut self) -> Option<Expr> {
        self.parse_name().map(Expr::EVar)
    }

    fn parse_integer(&mut self) -> Option<Expr> {
        self.parse_num().map(Expr::ENum)
    }

    fn parse_pack(&mut self) -> Result<Expr> {
        self.bump();
        if Some(Token::OpenBrace) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let x = if let Some(n) = self.parse_num() {
            n
        } else {
            return Err(ParseError::PlaceHolder);
        };
        if Some(Token::Comma) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let y = if let Some(n) = self.parse_num() {
            n
        } else {
            return Err(ParseError::PlaceHolder);
        };
        if Some(Token::CloseBrace) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        Ok(Expr::EConstr(x as usize, y as usize))
    }

    fn parse_paren(&mut self) -> Result<Expr> {
        self.bump();
        let expr = self.parse_expr()?;
        if Some(Token::CloseParen) == self.curr_token {
            self.bump();
            Ok(expr)
        } else {
            Err(ParseError::PlaceHolder)
        }
    }

    fn parse_let(&mut self, is_rec: bool) -> Result<Expr> {
        self.bump();
        let eqts = self.parse_eqts()?;
        if Some(Token::Keyword(Key::In)) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let body = self.parse_expr()?;
        if is_rec {
            Ok(Expr::ELetrec(eqts, Box::new(body)))
        } else {
            Ok(Expr::ELet(eqts, Box::new(body)))
        }
    }

    fn parse_eqts(&mut self) -> Result<Vec<LetEq>> {
        let mut eqts = Vec::new();
        while let Some(eqt) = self.parse_eqt()? {
            eqts.push(eqt);
        }
        Ok(eqts)
    }

    fn parse_eqt(&mut self) -> Result<Option<LetEq>> {
        if Some(Token::Keyword(Key::In)) == self.curr_token {
            return Ok(None);
        }
        let name = if let Some(name) = self.parse_name() {
            name
        } else {
            return Err(ParseError::PlaceHolder);
        };
        if Some(Token::Equal) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let body = self.parse_expr()?;
        if Some(Token::Semi) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        Ok(Some((name, body)))
    }

    fn parse_case(&mut self) -> Result<Expr> {
        self.bump();
        let expr = self.parse_expr()?;
        if Some(Token::Keyword(Key::Of)) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let alters = self.parse_alters()?;
        Ok(Expr::ECase(Box::new(expr), alters))
    }

    fn parse_alters(&mut self) -> Result<Vec<Alter>> {
        let mut alts = Vec::new();
        while let Some(alt) = self.parse_alt()? {
            alts.push(alt);
        }
        Ok(alts)
    }

    fn parse_alt(&mut self) -> Result<Option<Alter>> {
        if Some(Token::BinOp(BinOpToken::Lt)) == self.curr_token {
            self.bump();
        } else {
            return Ok(None);
        }
        let n = if let Some(num) = self.parse_num() {
            num
        } else {
            return Err(ParseError::PlaceHolder);
        };
        if Some(Token::BinOp(BinOpToken::Gt)) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let mut binds = Vec::new();
        while let Some(name) = self.parse_name() {
            binds.push(name);
        }
        if Some(Token::RightArrow) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let body = self.parse_expr()?;
        if Some(Token::Semi) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        Ok(Some((n as usize, binds, body)))
    }

    fn parse_lambda(&mut self) -> Result<Expr> {
        self.bump();
        let mut args = Vec::new();
        while let Some(name) = self.parse_name() {
            args.push(name);
        }
        if Some(Token::Dot) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let expr = self.parse_expr()?;
        Ok(Expr::ELam(args, Box::new(expr)))
    }

    fn parse_lhs(&mut self) -> Result<Expr> {
        let lhs = self.parse_apply()?;
        self.parse_binop_rhs(lhs, 40)
    }

    fn parse_binop_rhs(&mut self, mut lhs: Expr, min_precedence: u8) -> Result<Expr> {
        while self.is_binop_and_ge(min_precedence) {
            let op = self.get_op();
            self.bump();
            let mut rhs = self.parse_apply()?;
            while self.lookahead(op) {
                let min = get_precedence(self.get_op());
                rhs = self.parse_binop_rhs(rhs, min)?;
            }
            lhs = apply_binop(op, lhs, rhs);
        }
        Ok(lhs)
    }

    fn lookahead(&self, op: BinOpToken) -> bool {
        match self.curr_token {
            Some(Token::BinOp(binop)) => get_precedence(binop) > get_precedence(op),
            _ => false,
        }
    }

    fn get_op(&self) -> BinOpToken {
        match self.curr_token {
            Some(Token::BinOp(binop)) => binop,
            _ => unreachable!(),
        }
    }

    fn is_binop_and_ge(&self, min: u8) -> bool {
        match self.curr_token {
            Some(Token::BinOp(binop)) => get_precedence(binop) >= min,
            _ => false,
        }
    }
}

fn get_precedence(binop: BinOpToken) -> u8 {
    match binop {
        BinOpToken::Lt | BinOpToken::Le | BinOpToken::Gt | BinOpToken::Ge | BinOpToken::EqEq => 40,
        BinOpToken::Plus | BinOpToken::Minus => 50,
        BinOpToken::Star | BinOpToken::Slash => 60,
    }
}

fn apply_binop(op: BinOpToken, lhs: Expr, rhs: Expr) -> Expr {
    let evar_op = Expr::EVar(match op {
                                     BinOpToken::Plus => "+",
                                     BinOpToken::Minus => "-",
                                     BinOpToken::Star => "*",
                                     BinOpToken::Slash => "/",
                                     BinOpToken::Lt => "<",
                                     BinOpToken::Le => "<=",
                                     BinOpToken::Gt => ">",
                                     BinOpToken::Ge => ">=",
                                     BinOpToken::EqEq => "==",
                                 }
                                 .to_string());
    Expr::EAp(Box::new(Expr::EAp(Box::new(evar_op), Box::new(lhs))),
              Box::new(rhs))
}

#[derive(Debug)]
pub enum ParseError {
    Lex(lexer::LexError),
    PlaceHolder,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::Lex(ref err) => write!(f, "{}", err),
            ParseError::PlaceHolder => write!(f, "PlaceHolder"),
        }
    }
}

impl error::Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::Lex(ref err) => err.description(),
            ParseError::PlaceHolder => "PlaceHolder",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            ParseError::Lex(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<lexer::LexError> for ParseError {
    fn from(err: lexer::LexError) -> ParseError {
        ParseError::Lex(err)
    }
}
