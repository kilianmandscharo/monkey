use std::mem;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
struct ParsingError {
    message: String,
}

impl ParsingError {
    fn new(message: String) -> Self {
        ParsingError { message }
    }
}

type Result<T> = std::result::Result<T, ParsingError>;

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

enum Expression {
    Placeholder,
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

struct Identifier {
    token: Token,
}

struct Program {
    statements: Vec<Statement>,
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParsingError>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::new(TokenType::Illegal, 'x'),
            peek_token: Token::new(TokenType::Illegal, 'x'),
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };

        loop {
            match self.cur_token.t {
                TokenType::Eof => {
                    break;
                }
                _ => {
                    match self.parse_statement() {
                        Ok(statement) => program.statements.push(statement),
                        Err(error) => self.errors.push(error),
                    };
                    self.next_token();
                }
            }
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token.t {
            TokenType::Let => Ok(Statement::LetStatement(self.parse_let_statement()?)),
            _ => Err(ParsingError::new(
                "encountered unknown statement".to_string(),
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = mem::take(&mut self.cur_token);

        self.expect_peek(TokenType::Ident)?;

        let name = Identifier {
            token: mem::take(&mut self.cur_token),
        };

        self.expect_peek(TokenType::Assign)?;

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement {
            token,
            name,
            value: Expression::Placeholder,
        })
    }

    fn expect_peek(&mut self, t: TokenType) -> Result<()> {
        if self.peek_token_is(t) {
            self.next_token();
            return Ok(());
        }
        Err(ParsingError::new(format!(
            "expected token type '{:?}', received '{:?}'",
            t, self.peek_token.t
        )))
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        t == self.cur_token.t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        t == self.peek_token.t
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::lexer;

    use super::*;

    #[test]
    fn test_let_statement() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        if parser.errors.len() > 0 {
            for error in &parser.errors {
                println!("ERROR: {}", error.message);
            }
            panic!("found {} parsing errors", parser.errors.len());
        }

        assert_eq!(3, program.statements.len());

        let tests = vec!["x", "y", "foobar"];
        for (i, expected_identifier) in tests.into_iter().enumerate() {
            match &program.statements[i] {
                Statement::LetStatement(let_statement) => {
                    assert_eq!("let", let_statement.token.literal);
                    assert_eq!(expected_identifier, let_statement.name.token.literal);
                }
                _ => {
                    panic!("not a let statement");
                }
            }
        }
    }
}
