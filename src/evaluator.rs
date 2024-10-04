use crate::ast::Expression;
use crate::ast::Node;
use crate::ast::Statement;
use crate::object::{Integer, Object};

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Statement(statement) => match statement {
            Statement::ExpressionStatement(expression_statement) => {
                eval(Node::Expression(expression_statement.expression))
            }
            _ => panic!("not implemented"),
        },
        Node::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => Object::Integer(Integer {
                value: integer_literal.value,
            }),
            _ => panic!("not implemented"),
        },
    }
}

fn eval_statements(mut statements: Vec<Statement>) -> Object {
    eval(Node::Statement(statements.swap_remove(0)))
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{ast::Node, lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_integer_expression() {
        let tests: Vec<(&'static str, i64)> = vec![("5", 5), ("10", 10)];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().expect("to parse program");
        eval(Node::Program(program))
    }

    fn test_integer_object(obj: Object, expected: i64) {
        let integer_object = match obj {
            Object::Integer(integer) => integer,
            _ => panic!("not an integer object"),
        };
        assert_eq!(expected, integer_object.value);
    }
}
