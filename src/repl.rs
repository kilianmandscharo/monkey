use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<usize> {
    loop {
        print_prompt();
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let mut lexer = Lexer::new(&input);
        loop {
            match lexer.next_token() {
                Token {
                    t: TokenType::Eof,
                    literal: _,
                } => {
                    break;
                }
                token => {
                    println!("{:?}", token);
                }
            }
        }
    }
}

fn print_prompt() {
    print!("{PROMPT}");
    io::stdout().flush().unwrap();
}
