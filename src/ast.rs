use std::mem;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

enum Statement {
    LetStatement(LetStatement),
    ReturnStatement,
    IfStatement,
}

enum Expression {}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

struct Identifier {
    token: Token,
    value: String,
}

struct Program {
    statements: Vec<Statement>,
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::new(TokenType::Illegal, 'x'),
            peek_token: Token::new(TokenType::Illegal, 'x'),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }
}
