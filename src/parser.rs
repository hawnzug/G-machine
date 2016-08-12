use ast;
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

pub fn parse<P: AsRef<Path>>(file_path: P) -> Result<ast::Program> {
    let mut parser = Parser {
        tokens: try!(lexer::tokenize(file_path)),
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
    fn run(&mut self) -> Result<ast::Program> {
        let mut program = Vec::new();
        while let Ok(scdef) = self.parse_scdef() {
            program.push(scdef);
        }
        Ok(program)
    }

    fn parse_scdef(&mut self) -> Result<ast::ScDef> {
        let name = if let Some(ast::Expr::EVar(s)) = self.parse_ident() {
            s
        } else {
            return Err(ParseError::PlaceHolder);
        };
        let mut args = Vec::new();
        while let Some(ast::Expr::EVar(s)) = self.parse_ident() {
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
            Ok(ast::ScDef {
                name: name,
                args: args,
                body: body,
            })
        } else {
            Err(ParseError::PlaceHolder)
        }
    }

    fn parse_expr(&mut self) -> Result<ast::Expr> {
        match self.curr_token {
            Some(Token::Keyword(Key::Let)) => self.parse_let(false),
            Some(Token::Keyword(Key::Letrec)) => self.parse_let(true),
            Some(Token::Keyword(Key::Case)) => self.parse_case(),
            Some(Token::BackSlash) => self.parse_lambda(),
            _ => Ok(self.parse_integer().unwrap()),
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

    fn parse_ident(&mut self) -> Option<ast::Expr> {
        self.parse_name().map(ast::Expr::EVar)
    }

    fn parse_integer(&mut self) -> Option<ast::Expr> {
        self.parse_num().map(ast::Expr::ENum)
    }

    fn parse_let(&mut self, is_rec: bool) -> Result<ast::Expr> {
        self.bump();
        let eqts = try!(self.parse_eqts());
        if Some(Token::Keyword(Key::In)) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let body = try!(self.parse_expr());
        if is_rec {
            Ok(ast::Expr::ELetrec(eqts, Box::new(body)))
        } else {
            Ok(ast::Expr::ELet(eqts, Box::new(body)))
        }
    }

    fn parse_eqts(&mut self) -> Result<Vec<ast::LetEq>> {
        let mut eqts = Vec::new();
        while let Some(eqt) = try!(self.parse_eqt()) {
            eqts.push(eqt);
        }
        Ok(eqts)
        // Ok(vec![("x".to_string(), ast::Expr::ENum(42))])
    }

    fn parse_eqt(&mut self) -> Result<Option<ast::LetEq>> {
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
        let body = try!(self.parse_expr());
        if Some(Token::Semi) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        Ok(Some((name, body)))
    }

    fn parse_case(&mut self) -> Result<ast::Expr> {
        self.bump();
        let expr = try!(self.parse_expr());
        if Some(Token::Keyword(Key::Of)) == self.curr_token {
            self.bump();
        } else {
            return Err(ParseError::PlaceHolder);
        }
        let alters = try!(self.parse_alters());
        Ok(ast::Expr::ECase(Box::new(expr), alters))
    }

    fn parse_alters(&mut self) -> Result<Vec<ast::Alter>> {
        let mut alts = Vec::new();
        while let Some(alt) = try!(self.parse_alt()) {
            alts.push(alt);
        }
        Ok(alts)
    }

    fn parse_alt(&mut self) -> Result<Option<ast::Alter>> {
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
        let body = try!(self.parse_expr());
        Ok(Some((n, binds, body)))
    }

    fn parse_lambda(&mut self) -> Result<ast::Expr> {
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
        let expr = try!(self.parse_expr());
        Ok(ast::Expr::ELam(args, Box::new(expr)))
    }

    fn bump(&mut self) {
        self.curr_index += 1;
        if self.curr_index < self.tokens.len() {
            self.curr_token = Some(self.tokens[self.curr_index].token.clone());
        } else {
            self.curr_token = None;
        }
    }
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
