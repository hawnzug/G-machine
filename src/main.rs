mod ast;
mod parser;
mod printer;
mod gmachine;
mod lexer;
mod token;

use std::env;
use std::path::Path;

use parser::parse;
use printer::show_result;
use gmachine::{eval, compile};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} FILENAME", args[0]);
    } else {
        run_prog(&args[1]);
    }
}

fn run_prog<P: AsRef<Path>>(file_path: P) {
    show_result(eval(compile(parse(file_path))));
}
