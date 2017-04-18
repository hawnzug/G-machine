mod ast;
mod parser;
mod printer;
mod gmachine;
mod lexer;
mod token;
mod heap;
mod compiler;

use std::env;
use std::path::Path;

use parser::parse;
use printer::show_result;
use compiler::compile;
use gmachine::eval;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} FILENAME", args[0]);
    } else {
        run_prog(&args[1]);
    }
}

fn run_prog<P: AsRef<Path>>(file_path: P) {
    match parse(file_path) {
        Ok(prog) => {
            show_result(eval(compile(prog)));
        }
        Err(err) => println!("{}", err),
    }
}
