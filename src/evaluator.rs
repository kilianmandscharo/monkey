use crate::ast::Expression;
use crate::ast::IfExpression;
use crate::ast::Node;
use crate::ast::Statement;
use crate::object::{Boolean, Integer, Null, Object};

static TRUE: Object = Object::Boolean(Boolean { value: true });
static FALSE: Object = Object::Boolean(Boolean { value: false });
static NULL: Object = Object::Null(Null {});

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Statement(statement) => match statement {
            Statement::ExpressionStatement(expression_statement) => {
                eval(Node::Expression(expression_statement.expression))
            }
            Statement::BlockStatement(block_statement) => {
                eval_statements(block_statement.statements)
            }
            _ => panic!("not implemented"),
        },
        Node::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => Object::Integer(Integer {
                value: integer_literal.value,
            }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::PrefixExpression(prefix_expression) => {
                let right = eval(Node::Expression(*prefix_expression.right));
                eval_prefix_expression(&prefix_expression.operator, right)
            }
            Expression::InfixExpression(infix_expression) => {
                let left = eval(Node::Expression(*infix_expression.left));
                let right = eval(Node::Expression(*infix_expression.right));
                eval_infix_expression(&infix_expression.operator, left, right)
            }
            Expression::IfExpression(if_expression) => eval_if_expression(if_expression),
            _ => panic!("not implemented"),
        },
    }
}

fn eval_if_expression(if_expression: IfExpression) -> Object {
    let condition = eval(Node::Expression(*if_expression.condition));
    if is_truthy(condition) {
        eval(Node::Statement(Statement::BlockStatement(
            if_expression.consequence,
        )))
    } else if let Some(alternative) = if_expression.alternative {
        eval(Node::Statement(Statement::BlockStatement(alternative)))
    } else {
        NULL
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null(_) => false,
        Object::Boolean(boolean) => boolean.value,
        _ => true,
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_back_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL,
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if let Object::Integer(left) = left {
        if let Object::Integer(right) = right {
            return eval_integer_infix_expression(operator, left, right);
        }
    }
    if let Object::Boolean(left) = left {
        if let Object::Boolean(right) = right {
            return eval_boolean_infix_expression(operator, left, right);
        }
    }
    NULL
}

fn eval_boolean_infix_expression(operator: &str, left: Boolean, right: Boolean) -> Object {
    match operator {
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(operator: &str, left: Integer, right: Integer) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => NULL,
    }
}

fn native_bool_to_boolean_object(val: bool) -> Object {
    if val {
        TRUE
    } else {
        FALSE
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(Integer {
            value: integer.value * -1,
        }),
        _ => NULL,
    }
}

fn eval_back_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(boolean) => {
            if boolean.value {
                FALSE
            } else {
                TRUE
            }
        }
        Object::Null(_) => TRUE,
        _ => FALSE,
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
        let tests: Vec<(&'static str, i64)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(&'static str, bool)> = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: Vec<(&'static str, bool)> = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests: Vec<(&'static str, Option<i64>)> = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            if let Some(expected_int) = expected {
                test_integer_object(evaluated, expected_int);
            } else {
                test_null_object(evaluated);
            }
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

    fn test_null_object(obj: Object) {
        match obj {
            Object::Null(_) => {}
            _ => panic!("not a null object"),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        let boolean_object = match obj {
            Object::Boolean(boolean) => boolean,
            _ => panic!("not a boolean object"),
        };
        assert_eq!(expected, boolean_object.value);
    }
}
