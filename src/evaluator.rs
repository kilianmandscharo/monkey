use crate::ast::{Expression, Identifier, IfExpression, Node, Statement};
use crate::environment::Environment;
use crate::object::{Boolean, Function, Integer, Object, ReturnValue};

pub fn eval(node: Node, env: Environment) -> Object {
    match node {
        Node::Program(program) => eval_program(program.statements, env),
        Node::Statement(statement) => match statement {
            Statement::LetStatement(let_statement) => {
                let val = eval(Node::Expression(let_statement.value), env.clone());
                if val.is_error() {
                    return val;
                }
                env.set(&let_statement.name.token.literal, val)
            }
            Statement::ExpressionStatement(expression_statement) => {
                eval(Node::Expression(expression_statement.expression), env)
            }
            Statement::BlockStatement(block_statement) => {
                eval_block_statement(block_statement.statements, env)
            }
            Statement::ReturnStatement(return_statement) => {
                let value = eval(Node::Expression(return_statement.return_value), env);
                if value.is_error() {
                    value
                } else {
                    Object::ReturnValue(ReturnValue {
                        value: Box::new(value),
                    })
                }
            }
        },
        Node::Expression(expression) => match expression {
            Expression::IntegerLiteral(integer_literal) => Object::Integer(Integer {
                value: integer_literal.value,
            }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::PrefixExpression(prefix_expression) => {
                let right = eval(Node::Expression(*prefix_expression.right), env);
                if right.is_error() {
                    right
                } else {
                    eval_prefix_expression(&prefix_expression.operator, right)
                }
            }
            Expression::InfixExpression(infix_expression) => {
                let left = eval(Node::Expression(*infix_expression.left), env.clone());
                if left.is_error() {
                    return left;
                }
                let right = eval(Node::Expression(*infix_expression.right), env);
                if right.is_error() {
                    return right;
                }
                eval_infix_expression(&infix_expression.operator, left, right)
            }
            Expression::IfExpression(if_expression) => eval_if_expression(if_expression, env),
            Expression::Identifier(identifier) => eval_identifier(identifier, env),
            Expression::FunctionLiteral(function_literal) => Object::Function(Function {
                parameters: function_literal.parameters,
                env: env.clone(),
                body: function_literal.body,
            }),
            Expression::CallExpression(call_expression) => {
                let func = eval(Node::Expression(*call_expression.function), env.clone());
                if func.is_error() {
                    return func;
                }
                let mut args = eval_expressions(call_expression.arguments, env);
                if args.len() == 1 && args[0].is_error() {
                    args.swap_remove(0)
                } else {
                    apply_function(func, args)
                }
            }
            Expression::Empty() => panic!("you should not be here"),
        },
    }
}

fn apply_function(obj: Object, args: Vec<Object>) -> Object {
    let func = match obj {
        Object::Function(function) => function,
        _ => {
            return Object::new_error(format!("not a function: {}", obj.get_type()));
        }
    };
    let params = func.parameters;
    let env = func.env;
    let body = func.body;
    let extended_env = extend_function_env(params, env, args);
    let evaluated = eval(
        Node::Statement(Statement::BlockStatement(body)),
        extended_env,
    );
    unwrap_return_value(evaluated)
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::ReturnValue(return_value) => *return_value.value,
        _ => obj,
    }
}

fn extend_function_env(
    params: Vec<Identifier>,
    env: Environment,
    args: Vec<Object>,
) -> Environment {
    let env = Environment::new_enclosed(env);
    for (param, arg) in params.iter().zip(args.into_iter()) {
        env.set(&param.token.literal, arg);
    }
    env
}

fn eval_expressions(expressions: Vec<Expression>, env: Environment) -> Vec<Object> {
    let mut result = Vec::new();
    for expression in expressions {
        let evaluated = eval(Node::Expression(expression), env.clone());
        if evaluated.is_error() {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn eval_identifier(node: Identifier, env: Environment) -> Object {
    if let Some(val) = env.get(&node.token.literal) {
        val
    } else {
        Object::new_error(format!("identifier not found: {}", node.token.literal))
    }
}

fn eval_if_expression(if_expression: IfExpression, env: Environment) -> Object {
    let condition = eval(Node::Expression(*if_expression.condition), env.clone());
    if condition.is_error() {
        return condition;
    } else if is_truthy(condition) {
        eval(
            Node::Statement(Statement::BlockStatement(if_expression.consequence)),
            env,
        )
    } else if let Some(alternative) = if_expression.alternative {
        eval(Node::Statement(Statement::BlockStatement(alternative)), env)
    } else {
        Object::new_null()
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
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.get_type()
        )),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if let Object::Integer(left_integer) = &left {
        if let Object::Integer(right_integer) = &right {
            return eval_integer_infix_expression(operator, left_integer, right_integer);
        } else {
            return Object::new_error(format!(
                "type mismatch: {} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            ));
        }
    } else if let Object::Boolean(left_boolean) = &left {
        if let Object::Boolean(right_boolean) = &right {
            return eval_boolean_infix_expression(operator, left_boolean, right_boolean);
        } else {
            return Object::new_error(format!(
                "type mismatch: {} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            ));
        }
    }
    Object::new_error(format!(
        "type mismatch: {} {} {}",
        left.get_type(),
        operator,
        right.get_type()
    ))
}

fn eval_boolean_infix_expression(operator: &str, left: &Boolean, right: &Boolean) -> Object {
    match operator {
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Object::new_error(format!("unknown operator: Boolean {operator} Boolean")),
    }
}

fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Object::new_error(format!("unknown operator: Integer {operator} Integer")),
    }
}

fn native_bool_to_boolean_object(val: bool) -> Object {
    if val {
        Object::new_true()
    } else {
        Object::new_false()
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(Integer {
            value: integer.value * -1,
        }),
        _ => Object::new_error(format!("unknown operator: -{}", right.get_type())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(boolean) => native_bool_to_boolean_object(!boolean.value),
        Object::Null(_) => Object::new_true(),
        _ => Object::new_false(),
    }
}

fn eval_program(statements: Vec<Statement>, env: Environment) -> Object {
    let mut result = Object::new_null();
    for statement in statements {
        result = eval(Node::Statement(statement), env.clone());
        match result {
            Object::ReturnValue(return_value) => {
                return *return_value.value;
            }
            Object::Error(_) => {
                return result;
            }
            _ => {}
        }
    }
    result
}

fn eval_block_statement(statements: Vec<Statement>, env: Environment) -> Object {
    let mut result = Object::new_null();
    for statement in statements {
        result = eval(Node::Statement(statement), env.clone());
        match result {
            Object::ReturnValue(_) => {
                return result;
            }
            Object::Error(_) => {
                return result;
            }
            _ => {}
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{
        ast::Node, environment::Environment, lexer::Lexer, object::Object, parser::Parser,
    };

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

    #[test]
    fn test_return_statements() {
        let tests: Vec<(&'static str, i64)> = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
            (" let f = fn(x) { return x; x + 10; }; f(10);", 10),
            (
                "let f = fn(x) { let result = x + 10; return result; return 10; }; f(10);",
                20,
            ),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests: Vec<(&'static str, &'static str)> = vec![
            ("5 + true;", "type mismatch: Integer + Boolean"),
            ("5 + true; 5;", "type mismatch: Integer + Boolean"),
            ("-true", "unknown operator: -Boolean"),
            ("true + false;", "unknown operator: Boolean + Boolean"),
            ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean + Boolean",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            if let Object::Error(error) = evaluated {
                assert_eq!(expected, error.message);
            } else {
                panic!("no error object");
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests: Vec<(&'static str, i64)> = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for test in tests {
            let (input, expected) = test;
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        let function = match evaluated {
            Object::Function(function) => function,
            _ => panic!("not a function object"),
        };
        assert_eq!(1, function.parameters.len());
        assert_eq!("x", function.parameters[0].to_string());
        assert_eq!("(x + 2)", function.body.to_string());
    }

    #[test]
    fn test_function_application() {
        let tests: Vec<(&'static str, i64)> = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];
        for test in tests {
            let (input, expected) = test;
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_closures() {
        let input =
            "let a = 1; let newAdder = fn(x) { fn(y) { x + y + a }; }; let addTwo = newAdder(2); addTwo(2);";
        test_integer_object(test_eval(input), 5);
    }

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().expect("to parse program");
        let env = Environment::new();
        eval(Node::Program(program), env)
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
