mod ast;
mod parser;
mod gmachine;
mod lexer;
mod token;
mod heap;
mod compiler;

use std::env;
use std::path::Path;
use std::process;

use parser::parse;
use compiler::compile;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} FILENAME", args[0]);
    } else {
        run_prog(&args[1]);
    }
}

fn run_prog<P: AsRef<Path>>(file_path: P) {
    let prog = parse(file_path).unwrap_or_else(|err| {
                                                   println!("{}", err);
                                                   process::exit(1);
                                               });
    compile(prog)
        .run()
        .unwrap_or_else(|err| {
                            println!("{}", err);
                            process::exit(1);
                        })
        .show();
}
