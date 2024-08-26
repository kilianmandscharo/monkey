#![allow(dead_code)]

use std::{collections::HashMap, mem};

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, ParsingError,
        Program, Result, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn value(&self) -> usize {
        match *self {
            Precedence::Lowest => 1,
            Precedence::Equals => 2,
            Precedence::Lessgreater => 3,
            Precedence::Sum => 4,
            Precedence::Product => 5,
            Precedence::Prefix => 6,
            Precedence::Call => 7,
        }
    }
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Result<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Expression) -> Result<Expression>;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParsingError>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::new(TokenType::Illegal, 'x'),
            peek_token: Token::new(TokenType::Illegal, 'x'),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_prefix(&mut self, t: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(t, func);
    }

    fn register_infix(&mut self, t: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(t, func);
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

    fn parse_identifier(&mut self) -> Result<Expression> {
        Ok(Expression::Identifier(Identifier {
            token: mem::take(&mut self.cur_token),
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        if let Ok(value) = self.cur_token.literal.parse::<i64>() {
            return Ok(Expression::IntegerLiteral(IntegerLiteral {
                token: mem::take(&mut self.cur_token),
                value,
            }));
        }
        Err(ParsingError::new(format!(
            "failed to parse token literal '{}' as i64",
            self.cur_token.literal
        )))
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token.t {
            TokenType::Let => Ok(Statement::LetStatement(self.parse_let_statement()?)),
            TokenType::Return => Ok(Statement::ReturnStatement(self.parse_return_statement()?)),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expressions_statement()?,
            )),
        }
    }

    fn parse_expressions_statement(&mut self) -> Result<ExpressionStatement> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let token = mem::take(&mut self.cur_token);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ExpressionStatement { token, expression })
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression> {
        match self.prefix_parse_fns.get(&self.cur_token.t) {
            Some(prefix) => Ok(prefix(self)?),
            None => Err(ParsingError::new("no prefix function".to_string())),
        }
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        let token = mem::take(&mut self.cur_token);

        self.next_token();

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement {
            token,
            return_value: Expression::Placeholder("placeholder".to_string()),
        })
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
            value: Expression::Placeholder("placeholder".to_string()),
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

        check_parser_errors(&parser);

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

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(3, program.statements.len());

        for statement in &program.statements {
            match statement {
                Statement::ReturnStatement(_) => {}
                _ => {
                    panic!("not a return statement");
                }
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => match &stmt.expression {
                Expression::Identifier(ident) => {
                    assert_eq!("foobar", ident.token.literal);
                }
                _ => {
                    panic!("not an identifier expression");
                }
            },
            _ => {
                panic!("not an expression statement");
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => match &stmt.expression {
                Expression::IntegerLiteral(integer_literal) => {
                    assert_eq!(5, integer_literal.value);
                }
                _ => {
                    panic!("not an integer literal expression");
                }
            },
            _ => {
                panic!("not an expression statement");
            }
        }
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.len() > 0 {
            for error in &parser.errors {
                println!("ERROR: {}", error.message);
            }
            panic!("found {} parsing errors", parser.errors.len());
        }
    }
}
