#![allow(dead_code)]

use std::{collections::HashMap, mem};

use crate::{
    ast::{
        Boolean, Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral,
        LetStatement, ParsingError, PrefixExpression, Program, Result, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Clone, Copy, Debug)]
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

impl PartialEq for Precedence {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.value().cmp(&other.value()))
    }
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Result<Expression>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Box<Expression>) -> Result<Expression>;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParsingError>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
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
            precedences: HashMap::new(),
        };

        parser.register_precedences();

        parser.register_prefix(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::True, Parser::parse_boolean);
        parser.register_prefix(TokenType::False, Parser::parse_boolean);
        parser.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_precedences(&mut self) {
        self.precedences.insert(TokenType::Eq, Precedence::Equals);
        self.precedences
            .insert(TokenType::NotEq, Precedence::Equals);
        self.precedences
            .insert(TokenType::LT, Precedence::Lessgreater);
        self.precedences
            .insert(TokenType::GT, Precedence::Lessgreater);
        self.precedences.insert(TokenType::Plus, Precedence::Sum);
        self.precedences.insert(TokenType::Minus, Precedence::Sum);
        self.precedences
            .insert(TokenType::Slash, Precedence::Product);
        self.precedences
            .insert(TokenType::Asterisk, Precedence::Product);
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedences.get(&self.peek_token.t) {
            return precedence.clone();
        }
        Precedence::Lowest
    }

    fn cur_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedences.get(&self.cur_token.t) {
            return precedence.clone();
        }
        Precedence::Lowest
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

    fn parse_boolean(&mut self) -> Result<Expression> {
        let value = self.cur_token_is(TokenType::True);
        let token = mem::take(&mut self.cur_token);
        Ok(Expression::Boolean(Boolean { token, value }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let token = mem::take(&mut self.cur_token);
        let operator = token.literal.clone();
        let right = Box::new(Expression::Placeholder());

        let mut expression = PrefixExpression {
            token,
            operator,
            right,
        };

        self.next_token();

        expression.right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(Expression::PrefixExpression(expression))
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Expression> {
        let precedence = self.cur_precedence();
        let token = mem::take(&mut self.cur_token);
        let operator = token.literal.clone();
        let right = Box::new(Expression::Placeholder());

        let mut expression = InfixExpression {
            token,
            operator,
            right,
            left,
        };

        self.next_token();

        expression.right = Box::new(self.parse_expression(precedence)?);

        Ok(Expression::InfixExpression(expression))
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

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let prefix = match self.prefix_parse_fns.get(&self.cur_token.t) {
            Some(prefix) => prefix,
            None => {
                return Err(ParsingError::new(format!(
                    "no prefix function for {:?}",
                    self.cur_token.t
                )))
            }
        };

        let mut left = prefix(self)?;

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix = match self.infix_parse_fns.get(&self.peek_token.t) {
                Some(infix) => infix.clone(),
                None => return Ok(left),
            };

            self.next_token();
            left = infix(self, Box::new(left))?;
        }

        Ok(left)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        let token = mem::take(&mut self.cur_token);

        self.next_token();

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement {
            token,
            return_value: Expression::Placeholder(),
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
            value: Expression::Placeholder(),
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
    use std::any::Any;

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
            Statement::ExpressionStatement(stmt) => test_identifier(&stmt.expression, "foobar"),
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
            Statement::ExpressionStatement(stmt) => test_integer_literal(&stmt.expression, 5),
            _ => {
                panic!("not an expression statement");
            }
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => test_boolean_literal(&stmt.expression, true),
            _ => {
                panic!("not an expression statement");
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        type Test = (&'static str, &'static str, &'static dyn Any);

        let tests: Vec<Test> = vec![
            ("!5", "!", &5),
            ("-15", "-", &15),
            ("!true", "!", &true),
            ("!false", "!", &false),
        ];

        for test in tests {
            let (input, operator, value) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            match &program.statements[0] {
                Statement::ExpressionStatement(stmt) => {
                    test_prefix_expression(&stmt.expression, operator, value);
                }
                _ => {
                    panic!("not an expression statement");
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        type Test = (
            &'static str,
            &'static dyn Any,
            &'static str,
            &'static dyn Any,
        );

        let tests: Vec<Test> = vec![
            ("5 + 5;", &5, "+", &5),
            ("5 - 5;", &5, "-", &5),
            ("5 * 5;", &5, "*", &5),
            ("5 / 5;", &5, "/", &5),
            ("5 > 5;", &5, ">", &5),
            ("5 < 5;", &5, "<", &5),
            ("5 == 5;", &5, "==", &5),
            ("5 != 5;", &5, "!=", &5),
            ("true == true", &true, "==", &true),
            ("true != false", &true, "!=", &false),
            ("false == false", &false, "==", &false),
        ];

        for test in tests {
            let (input, left_value, operator, right_value) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            match &program.statements[0] {
                Statement::ExpressionStatement(ref stmt) => {
                    test_infix_expression(&stmt.expression, left_value, operator, right_value);
                }
                _ => {
                    panic!("not an expression statement");
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        type Test = (&'static str, &'static str);

        let tests: Vec<Test> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for test in tests {
            let (input, expected) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            let actual = program.to_string();

            assert_eq!(expected, actual);
        }
    }

    fn test_integer_literal(integer_literal: &Expression, value: i64) {
        match *integer_literal {
            Expression::IntegerLiteral(ref integer_literal) => {
                assert_eq!(integer_literal.value, value);
                assert_eq!(integer_literal.token.literal, value.to_string());
            }
            _ => panic!("not an integer literal"),
        }
    }

    fn test_boolean_literal(boolean: &Expression, value: bool) {
        match *boolean {
            Expression::Boolean(ref boolean) => {
                assert_eq!(boolean.value, value);
                assert_eq!(boolean.token.literal, value.to_string());
            }
            _ => panic!("not a boolean"),
        }
    }

    fn test_identifier(identifier: &Expression, value: &str) {
        match *identifier {
            Expression::Identifier(ref identifier) => {
                assert_eq!(identifier.token.literal, value);
            }
            _ => panic!("not an identifier"),
        }
    }

    fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
        if let Some(integer) = expected.downcast_ref::<i64>() {
            test_integer_literal(expression, *integer);
            return;
        }
        if let Some(integer) = expected.downcast_ref::<i32>() {
            test_integer_literal(expression, *integer as i64);
            return;
        }
        if let Some(string) = expected.downcast_ref::<&str>() {
            test_identifier(expression, string);
            return;
        }
        if let Some(boolean) = expected.downcast_ref::<bool>() {
            test_boolean_literal(expression, *boolean);
            return;
        }
        panic!("type of expected can't be handled");
    }

    fn test_infix_expression(
        expression: &Expression,
        left: &dyn Any,
        operator: &str,
        right: &dyn Any,
    ) {
        match *expression {
            Expression::InfixExpression(ref infix_expression) => {
                test_literal_expression(&infix_expression.left, left);
                assert_eq!(operator, infix_expression.operator);
                test_literal_expression(&infix_expression.right, right);
            }
            _ => panic!("not an infix expression"),
        }
    }

    fn test_prefix_expression(expression: &Expression, operator: &str, right: &dyn Any) {
        match *expression {
            Expression::PrefixExpression(ref prefix_expression) => {
                assert_eq!(operator, prefix_expression.operator);
                test_literal_expression(&prefix_expression.right, right);
            }
            _ => panic!("not an infix expression"),
        }
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.len() > 0 {
            eprintln!("parser has {} errors", parser.errors.len());
            for error in &parser.errors {
                eprintln!("ERROR: {}", error.message);
            }
            panic!("found {} parsing errors", parser.errors.len());
        }
    }
}
