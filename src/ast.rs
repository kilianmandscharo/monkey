#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug, Clone)]
pub struct ParsingError {
    pub message: String,
}

impl ParsingError {
    pub fn new(message: String) -> Self {
        ParsingError { message }
    }
}

pub type Result<T> = std::result::Result<T, ParsingError>;

enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| statement.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.to_string()
            }
        };
        write!(f, "{}", content)
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            self.name.to_string(),
            self.value.to_string()
        )
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {};",
            self.token.literal,
            self.return_value.to_string()
        )
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

pub struct Identifier {
    pub token: Token,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

pub enum Expression {
    Placeholder(String),
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Expression::Placeholder(_) => "placeholder".to_string(),
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.to_string(),
        };
        write!(f, "{}", content)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: Token::from_str(TokenType::Let, "let"),
                name: Identifier {
                    token: Token::from_str(TokenType::Ident, "myVar"),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::from_str(TokenType::Ident, "anotherVar"),
                }),
            })],
        };
        assert_eq!("let myVar = anotherVar;", program.to_string());
    }
}
