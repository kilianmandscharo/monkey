use environment::Environment;

use crate::ast::Node;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::{env, fs::read_to_string, path::Path};

mod ast;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let mut args = env::args();
    if args.len() > 1 {
        args.next();
        let file_path = args.next().expect("to read file path arg");
        let extension = Path::new(&file_path)
            .extension()
            .expect("to read file extension")
            .to_str()
            .expect("to have valid file extension");
        assert_eq!(
            extension, "monk",
            "found invalid file extension: {extension}, file extension has to be 'monk'",
        );
        let input = read_to_string(file_path).expect("to read file");
        let mut parser = Parser::new(Lexer::new(&input));
        let program = parser.parse_program().expect("to parse program");
        if parser.errors.len() > 0 {
            eprintln!("Looks like we ran into some monkey business here...");
            parser.print_errors();
        }
        let evaluator = Evaluator::new();
        evaluator.eval(Node::Program(program), Environment::new());
    } else {
        println!("Hello, this is the Monkey programming language!");
        println!("Feel free to type in commands");
        repl::start().unwrap();
    }
}
