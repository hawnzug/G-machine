use token::*;

use std::fs::File;
use std::path::Path;
use std::io::Read;
use std::str::Chars;

pub struct Lexer {
    pub tokens: Vec<TokenAndPos>,
}

impl Lexer {
    pub fn new<P: AsRef<Path>>(file_path: P) -> Self {
        let mut file = File::open(file_path).unwrap();
        let mut content: String = String::new();
        file.read_to_string(&mut content).unwrap();
        let file_reader = StringReader {
            pos: Pos::new(1, 0),
            iter: content.chars(),
            curr: None,
        };
        Lexer { tokens: file_reader.tokenize() }
    }
}

struct StringReader<'a> {
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

    fn tokenize(mut self) -> Vec<TokenAndPos> {
        let mut tokens = Vec::new();
        self.bump();
        loop {
            if self.curr.is_none() {
                break;
            }
            match self.curr.unwrap() {
                ' ' => self.bump(),
                '\r' => {
                    self.bump();
                    if self.curr != Some('\n') {
                        unimplemented!();
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
                    let n = number.parse().unwrap();
                    tokens.push(TokenAndPos {
                        token: Token::Literal(Lit::Integer(n)),
                        pos: start,
                    });
                }
                '(' => {
                    tokens.push(TokenAndPos {
                        token: Token::OpenParen,
                        pos: self.pos,
                    })
                }
                ')' => {
                    tokens.push(TokenAndPos {
                        token: Token::CloseParen,
                        pos: self.pos,
                    })
                }
                ';' => {
                    tokens.push(TokenAndPos {
                        token: Token::Semi,
                        pos: self.pos,
                    })
                }
                '+' => {
                    tokens.push(TokenAndPos {
                        token: Token::BinOp(BinOpToken::Plus),
                        pos: self.pos,
                    })
                }
                '-' => {
                    tokens.push(TokenAndPos {
                        token: Token::BinOp(BinOpToken::Minus),
                        pos: self.pos,
                    })
                }
                '*' => {
                    tokens.push(TokenAndPos {
                        token: Token::BinOp(BinOpToken::Star),
                        pos: self.pos,
                    })
                }
                '/' => {
                    tokens.push(TokenAndPos {
                        token: Token::BinOp(BinOpToken::Slash),
                        pos: self.pos,
                    })
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
        tokens
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer() {}
}
