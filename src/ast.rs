use crate::token::Token;
use std::collections::HashMap;

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

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Node::Program(program) => program.to_string(),
            Node::Statement(statement) => statement.to_string(),
            Node::Expression(expression) => expression.to_string(),
        };
        write!(f, "{content}")
    }
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
                .join("")
        )
    }
}

#[derive(Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.to_string()
            }
            Statement::BlockStatement(block_statement) => block_statement.to_string(),
        };
        write!(f, "{}", content)
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| statement.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
}

impl Identifier {
    pub fn value(&self) -> &str {
        &self.token.literal
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Clone, PartialEq, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

#[derive(Clone, PartialEq, Hash)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, PartialEq, Hash)]
pub struct StringLiteral {
    pub token: Token,
}

impl std::fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

#[derive(Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.alternative {
            Some(ref alternative) => write!(
                f,
                "if {} {} else {}",
                self.condition.to_string(),
                self.consequence.to_string(),
                alternative.to_string(),
            ),
            None => write!(
                f,
                "if {} {}",
                self.condition.to_string(),
                self.consequence.to_string(),
            ),
        }
    }
}

#[derive(Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl std::fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}) {}",
            self.token.literal,
            self.parameters
                .iter()
                .map(|parameter| parameter.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.to_string(),
        )
    }
}

#[derive(Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl std::fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|exp| exp.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function.to_string(),
            self.arguments
                .iter()
                .map(|parameter| parameter.to_string())
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

#[derive(Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl std::fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Clone)]
pub struct MapLiteral {
    pub token: Token,
    pub pairs: HashMap<HashableExpression, Expression>,
}

impl std::fmt::Display for MapLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|pair| format!("{}:{}", pair.0.to_string(), pair.1.to_string()))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Clone, PartialEq, Hash)]
pub enum HashableExpression {
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Boolean(Boolean),
}

impl HashableExpression {
    pub fn to_expression(self) -> Expression {
        match self {
            HashableExpression::Boolean(boolean) => Expression::Boolean(boolean),
            HashableExpression::StringLiteral(string_literal) => {
                Expression::StringLiteral(string_literal)
            }
            HashableExpression::IntegerLiteral(integer_literal) => {
                Expression::IntegerLiteral(integer_literal)
            }
        }
    }
}

impl Eq for HashableExpression {}

impl std::fmt::Display for HashableExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            HashableExpression::IntegerLiteral(integer_literal) => integer_literal.to_string(),
            HashableExpression::Boolean(boolean) => boolean.to_string(),
            HashableExpression::StringLiteral(string_literal) => string_literal.to_string(),
        };
        write!(f, "{}", content)
    }
}

#[derive(Clone)]
pub enum Expression {
    Empty(),
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
    MapLiteral(MapLiteral),
}

impl Expression {
    pub fn to_hashable_expression(self) -> Result<HashableExpression> {
        match self {
            Expression::StringLiteral(string_literal) => {
                Ok(HashableExpression::StringLiteral(string_literal))
            }
            Expression::Boolean(boolean) => Ok(HashableExpression::Boolean(boolean)),
            Expression::IntegerLiteral(integer_literal) => {
                Ok(HashableExpression::IntegerLiteral(integer_literal))
            }
            _ => Err(ParsingError {
                message: format!(
                    "can't transform to HashableExpression: {}",
                    self.to_string()
                ),
            }),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Expression::Empty() => "empty".to_string(),
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.to_string(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.to_string(),
            Expression::InfixExpression(infix_expression) => infix_expression.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::IfExpression(if_expression) => if_expression.to_string(),
            Expression::FunctionLiteral(function_literal) => function_literal.to_string(),
            Expression::CallExpression(call_expression) => call_expression.to_string(),
            Expression::StringLiteral(string_literal) => string_literal.to_string(),
            Expression::ArrayLiteral(array_literal) => array_literal.to_string(),
            Expression::IndexExpression(index_expression) => index_expression.to_string(),
            Expression::MapLiteral(map_literal) => map_literal.to_string(),
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
                token: Token::from_string(TokenType::Let, "let".to_string()),
                name: Identifier {
                    token: Token::from_string(TokenType::Ident, "myVar".to_string()),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::from_string(TokenType::Ident, "anotherVar".to_string()),
                }),
            })],
        };
        assert_eq!("let myVar = anotherVar;", program.to_string());
    }
}
