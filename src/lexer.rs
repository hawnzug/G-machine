use token::*;

use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::str::Chars;
use std::num;
use std::fmt;
use std::error;
use std::io;

pub fn tokenize<P: AsRef<Path>>(file_path: P) -> Result<Vec<TokenAndPos>, LexError> {
    let mut file = File::open(file_path)?;
    let mut content: String = String::new();
    file.read_to_string(&mut content)?;
    let file_reader = StringReader {
        pos: Pos::new(1, 0),
        iter: content.chars(),
        curr: None,
    };
    file_reader.tokenize()
}

pub struct StringReader<'a> {
    pos: Pos,
    iter: Chars<'a>,
    curr: Option<char>,
}

impl<'a> StringReader<'a> {
    fn bump(&mut self) {
        if self.curr == Some('\n') {
            self.pos.line += 1;
            self.pos.column = 1;
        } else {
            self.pos.column += 1;
        }
        self.curr = self.iter.next();
    }

    fn tokenize(mut self) -> Result<Vec<TokenAndPos>, LexError> {
        let mut tokens = Vec::new();
        self.bump();
        loop {
            if self.curr.is_none() {
                break;
            }
            match self.curr.unwrap() {
                '\r' => {
                    self.bump();
                    if self.curr != Some('\n') {
                        return Err(LexError::Singler);
                    }
                }
                c if c.is_alphabetic() => {
                    let mut ident = String::new();
                    ident.push(c);
                    let start = self.pos;
                    loop {
                        self.bump();
                        match self.curr {
                            Some(c) if c.is_alphanumeric() => ident.push(c),
                            _ => break,
                        }
                    }
                    tokens.push(TokenAndPos {
                                    token: match ident.as_str() {
                                        "let" => Token::Keyword(Key::Let),
                                        "letrec" => Token::Keyword(Key::Letrec),
                                        "case" => Token::Keyword(Key::Case),
                                        "in" => Token::Keyword(Key::In),
                                        "of" => Token::Keyword(Key::Of),
                                        "pack" => Token::Keyword(Key::Pack),
                                        _ => Token::Ident(ident),
                                    },
                                    pos: start,
                                });
                }
                c if c.is_digit(10) => {
                    let mut number = String::new();
                    number.push(c);
                    let start = self.pos;
                    loop {
                        self.bump();
                        match self.curr {
                            Some(c) if c.is_digit(10) => number.push(c),
                            _ => break,
                        }
                    }
                    let n = number.parse()?;
                    tokens.push(TokenAndPos {
                                    token: Token::Literal(Lit::Integer(n)),
                                    pos: start,
                                });
                }
                '(' => {
                    tokens.push(TokenAndPos {
                                    token: Token::OpenParen,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                ')' => {
                    tokens.push(TokenAndPos {
                                    token: Token::CloseParen,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '{' => {
                    tokens.push(TokenAndPos {
                                    token: Token::OpenBrace,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '}' => {
                    tokens.push(TokenAndPos {
                                    token: Token::CloseBrace,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '\\' => {
                    tokens.push(TokenAndPos {
                                    token: Token::BackSlash,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '.' => {
                    tokens.push(TokenAndPos {
                                    token: Token::Dot,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                ';' => {
                    tokens.push(TokenAndPos {
                                    token: Token::Semi,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                ',' => {
                    tokens.push(TokenAndPos {
                                    token: Token::Comma,
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '+' => {
                    tokens.push(TokenAndPos {
                                    token: Token::BinOp(BinOpToken::Plus),
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '-' => {
                    let start = self.pos;
                    self.bump();
                    tokens.push(TokenAndPos {
                                    token: if self.curr == Some('>') {
                                        self.bump();
                                        Token::RightArrow
                                    } else {
                                        Token::BinOp(BinOpToken::Minus)
                                    },
                                    pos: start,
                                });
                }
                '*' => {
                    tokens.push(TokenAndPos {
                                    token: Token::BinOp(BinOpToken::Star),
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '/' => {
                    tokens.push(TokenAndPos {
                                    token: Token::BinOp(BinOpToken::Slash),
                                    pos: self.pos,
                                });
                    self.bump();
                }
                '=' => {
                    let start = self.pos;
                    self.bump();
                    tokens.push(TokenAndPos {
                                    token: if self.curr == Some('=') {
                                        self.bump();
                                        Token::BinOp(BinOpToken::EqEq)
                                    } else {
                                        Token::Equal
                                    },
                                    pos: start,
                                })
                }
                '<' => {
                    let start = self.pos;
                    self.bump();
                    tokens.push(TokenAndPos {
                                    token: if self.curr == Some('=') {
                                        self.bump();
                                        Token::BinOp(BinOpToken::Le)
                                    } else {
                                        Token::BinOp(BinOpToken::Lt)
                                    },
                                    pos: start,
                                })
                }
                '>' => {
                    let start = self.pos;
                    self.bump();
                    tokens.push(TokenAndPos {
                                    token: if self.curr == Some('=') {
                                        self.bump();
                                        Token::BinOp(BinOpToken::Ge)
                                    } else {
                                        Token::BinOp(BinOpToken::Gt)
                                    },
                                    pos: start,
                                })
                }
                _ => self.bump(),
            }
        }
        Ok(tokens)
    }
}

#[derive(Debug)]
pub enum LexError {
    FileToString(io::Error),
    ReadInt(num::ParseIntError),
    Singler,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexError::ReadInt(ref err) => write!(f, "Parse integer error: {}", err),
            LexError::FileToString(ref err) => write!(f, "File error: {}", err),
            LexError::Singler => write!(f, "There must be \\n after \\r"),
        }
    }
}

impl error::Error for LexError {
    fn description(&self) -> &str {
        match *self {
            LexError::ReadInt(ref err) => err.description(),
            LexError::FileToString(ref err) => err.description(),
            LexError::Singler => "No \\n after \\r",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            LexError::ReadInt(ref err) => Some(err),
            LexError::FileToString(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<num::ParseIntError> for LexError {
    fn from(err: num::ParseIntError) -> LexError {
        LexError::ReadInt(err)
    }
}

impl From<io::Error> for LexError {
    fn from(err: io::Error) -> LexError {
        LexError::FileToString(err)
    }
}
