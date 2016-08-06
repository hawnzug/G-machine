use ast;
use lexer;

use std::path::Path;

pub fn parse<P: AsRef<Path>>(file_path: P) -> ast::Program {
    let lex = lexer::Lexer::new(file_path);
    println!("{:?}", lex.tokens);

    vec![ast::ScDef {
             name: "main".to_string(),
             args: vec![],
             body: ast::Expr::ENum(42),
         }]
}
