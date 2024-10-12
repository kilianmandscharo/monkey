use crate::ast::Node;
use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<usize> {
    let env = Environment::new();
    loop {
        print_prompt();
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let mut parser = Parser::new(Lexer::new(&input));
        let program = parser.parse_program().expect("to parse program");
        if parser.errors.len() > 0 {
            eprintln!("Looks like we ran into some monkey business here...");
            parser.print_errors();
            continue;
        }
        let evaluator = Evaluator::new();
        evaluator.eval(Node::Program(program), env.clone());
    }
}

fn print_prompt() {
    print!("{PROMPT}");
    io::stdout().flush().unwrap();
}
