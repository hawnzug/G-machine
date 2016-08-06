use ast;
use lexer;

use std::path::Path;

pub fn parse<P: AsRef<Path>>(file_path: P) -> ast::Program {
    match lexer::Lexer::new(file_path) {
        Ok(lex) => println!("{:?}", lex.tokens),
        Err(err) => println!("{}", err),
    }

    vec![ast::ScDef {
             name: "main".to_string(),
             args: vec![],
             body: ast::Expr::ENum(42),
         }]
}
