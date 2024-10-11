use std::{collections::HashMap, mem};

use crate::{
    ast::{
        BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
        Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, ParsingError,
        PrefixExpression, Program, Result, ReturnStatement, Statement, StringLiteral,
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

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParsingError>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    precedences: HashMap<TokenType, Precedence>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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
        parser.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::If, Parser::parse_if_expression);
        parser.register_prefix(TokenType::Function, Parser::parse_function_literal);
        parser.register_prefix(TokenType::String, Parser::parse_string_literal);

        parser.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NotEq, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LParen, Parser::parse_call_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn print_errors(&self) {
        eprintln!("parser has {} errors", self.errors.len());
        for error in &self.errors {
            eprintln!("ERROR: {}", error.message);
        }
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
        self.precedences.insert(TokenType::LParen, Precedence::Call);
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

    pub fn parse_program(&mut self) -> Result<Program> {
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

    fn parse_string_literal(&mut self) -> Result<Expression> {
        Ok(Expression::StringLiteral(StringLiteral {
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
        let right = Box::new(Expression::Empty());

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
        let right = Box::new(Expression::Empty());

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

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest);
        match self.expect_peek(TokenType::RParen) {
            Ok(_) => expression,
            Err(_) => Ok(Expression::Empty()),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        let token = mem::take(&mut self.cur_token);

        self.expect_peek(TokenType::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::RParen)?;
        self.expect_peek(TokenType::LBrace)?;

        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            self.expect_peek(TokenType::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::IfExpression(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        let token = mem::take(&mut self.cur_token);
        self.expect_peek(TokenType::LParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(TokenType::LBrace)?;
        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier {
            token: mem::take(&mut self.cur_token),
        });

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier {
                token: mem::take(&mut self.cur_token),
            });
        }

        self.expect_peek(TokenType::RParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Box<Expression>) -> Result<Expression> {
        Ok(Expression::CallExpression(CallExpression {
            token: mem::take(&mut self.cur_token),
            arguments: self.parse_call_arguments()?,
            function,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenType::RParen)?;

        Ok(args)
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

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut block_statement = BlockStatement {
            token: mem::take(&mut self.cur_token),
            statements: Vec::new(),
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            let statement = self.parse_statement()?;
            block_statement.statements.push(statement);
            self.next_token();
        }

        Ok(block_statement)
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
        let return_value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }
        Ok(ReturnStatement {
            token,
            return_value,
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let token = mem::take(&mut self.cur_token);
        self.expect_peek(TokenType::Ident)?;
        let name = Identifier {
            token: mem::take(&mut self.cur_token),
        };
        self.expect_peek(TokenType::Assign)?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenType::Semicolon)?;
        Ok(LetStatement { token, name, value })
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
    fn test_let_statements() {
        type Test = (&'static str, &'static str, &'static dyn Any);

        let tests: Vec<Test> = vec![
            ("let x = 5;", "x", &5),
            ("let y = true;", "y", &true),
            ("let foobar = y;", "foobar", &"y"),
        ];

        for test in tests {
            let (input, expected_identifier, expected_value) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            test_let_statement(&program.statements[0], expected_identifier);

            match &program.statements[0] {
                Statement::LetStatement(let_statement) => {
                    test_literal_expression(&let_statement.value, expected_value)
                }
                _ => panic!("not a let statement"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        type Test = (&'static str, &'static dyn Any);

        let tests: Vec<Test> = vec![
            ("return 5;", &5),
            ("return true", &true),
            ("return foobar", &"foobar"),
        ];

        for test in tests {
            let (input, expected_value) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            let return_statement = match &program.statements[0] {
                Statement::ReturnStatement(return_statement) => return_statement,
                _ => panic!("not a return statement"),
            };

            assert_eq!("return", return_statement.token.literal);

            test_literal_expression(&return_statement.return_value, expected_value);
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        let expression = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => &stmt.expression,
            _ => panic!("not an expression statement"),
        };

        let if_expression = match expression {
            Expression::IfExpression(if_expression) => if_expression,
            _ => panic!("not an if expression"),
        };

        test_infix_expression(&if_expression.condition, &"x", "<", &"y");
        assert_eq!(1, if_expression.consequence.statements.len());

        match if_expression.consequence.statements[0] {
            Statement::ExpressionStatement(ref expression_statement) => {
                test_identifier(&expression_statement.expression, "x");
            }
            _ => panic!("not an expression statement"),
        };

        if let Some(_) = if_expression.alternative {
            panic!("alternative statements were not None")
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        let expression = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => &stmt.expression,
            _ => panic!("not an expression statement"),
        };

        let if_expression = match expression {
            Expression::IfExpression(if_expression) => if_expression,
            _ => panic!("not an if expression"),
        };

        test_infix_expression(&if_expression.condition, &"x", "<", &"y");
        assert_eq!(1, if_expression.consequence.statements.len());

        match if_expression.consequence.statements[0] {
            Statement::ExpressionStatement(ref expression_statement) => {
                test_identifier(&expression_statement.expression, "x");
            }
            _ => panic!("not an expression statement"),
        };

        let alternative = match &if_expression.alternative {
            Some(alternative) => alternative,
            None => panic!("no alternative found"),
        };
        assert_eq!(1, alternative.statements.len());

        match alternative.statements[0] {
            Statement::ExpressionStatement(ref expression_statement) => {
                test_identifier(&expression_statement.expression, "y");
            }
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y }";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        let expression = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => &stmt.expression,
            _ => panic!("not an expression statement"),
        };

        let function_literal = match expression {
            Expression::FunctionLiteral(function_literal) => function_literal,
            _ => panic!("not a function literal"),
        };

        assert_eq!(2, function_literal.parameters.len());

        test_literal_expression(
            &Expression::Identifier(function_literal.parameters[0].clone()),
            &"x",
        );
        test_literal_expression(
            &Expression::Identifier(function_literal.parameters[1].clone()),
            &"y",
        );

        assert_eq!(1, function_literal.body.statements.len());

        let body_statement = match &function_literal.body.statements[0] {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("not an expression statement"),
        };

        test_infix_expression(&body_statement.expression, &"x", "+", &"y")
    }

    #[test]
    fn test_function_parameter_parsing() {
        type Test = (&'static str, Vec<&'static str>);

        let tests: Vec<Test> = vec![
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"]),
        ];

        for test in tests {
            let (input, expected_params) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            let expression = match &program.statements[0] {
                Statement::ExpressionStatement(stmt) => &stmt.expression,
                _ => panic!("not an expression statement"),
            };

            let function_literal = match expression {
                Expression::FunctionLiteral(function_literal) => function_literal,
                _ => panic!("not an function literal"),
            };

            assert_eq!(expected_params.len(), function_literal.parameters.len());

            for (i, identifier) in expected_params.into_iter().enumerate() {
                test_literal_expression(
                    &Expression::Identifier(function_literal.parameters[i].clone()),
                    &identifier,
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        let expression = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => &stmt.expression,
            _ => panic!("not an expression statement"),
        };

        let call_expression = match expression {
            Expression::CallExpression(call_expression) => call_expression,
            _ => panic!("not a call expression"),
        };

        test_identifier(&call_expression.function, "add");

        assert_eq!(3, call_expression.arguments.len());

        test_literal_expression(&call_expression.arguments[0], &1);
        test_infix_expression(&call_expression.arguments[1], &2, "*", &3);
        test_infix_expression(&call_expression.arguments[2], &4, "+", &5);
    }

    #[test]
    fn test_call_expression_argument_parsing() {
        type Test = (&'static str, &'static str, Vec<&'static str>);

        let tests: Vec<Test> = vec![
            ("add();", "add", vec![]),
            ("add(1);", "add", vec!["1"]),
            (
                "add(1, 2 * 3, 4 + 5);",
                "add",
                vec!["1", "(2 * 3)", "(4 + 5)"],
            ),
        ];

        for test in tests {
            let (input, expected_identifier, expected_args) = test;

            let lexer = lexer::Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("program to parse");

            check_parser_errors(&parser);

            assert_eq!(1, program.statements.len());

            let expression = match &program.statements[0] {
                Statement::ExpressionStatement(stmt) => &stmt.expression,
                _ => panic!("not an expression statement"),
            };

            let call_expression = match expression {
                Expression::CallExpression(call_expression) => call_expression,
                _ => panic!("not a call expression"),
            };

            test_identifier(&call_expression.function, expected_identifier);

            assert_eq!(expected_args.len(), call_expression.arguments.len());

            for (i, arg) in expected_args.into_iter().enumerate() {
                assert_eq!(arg, call_expression.arguments[i].to_string());
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("program to parse");

        check_parser_errors(&parser);

        assert_eq!(1, program.statements.len());

        let expression = match &program.statements[0] {
            Statement::ExpressionStatement(stmt) => &stmt.expression,
            _ => panic!("not an expression statement"),
        };

        let string_literal = match expression {
            Expression::StringLiteral(string_literal) => string_literal,
            _ => panic!("not a call expression"),
        };

        assert_eq!("hello world", string_literal.token.literal);
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

    fn test_let_statement(statement: &Statement, name: &str) {
        let let_statement = match statement {
            Statement::LetStatement(let_statement) => let_statement,
            _ => panic!("not a let statement"),
        };

        assert_eq!("let", let_statement.token.literal);
        assert_eq!(name, let_statement.name.token.literal);
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.len() > 0 {
            parser.print_errors();
            panic!("found {} parsing errors", parser.errors.len());
        }
    }
}
