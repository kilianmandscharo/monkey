use crate::ast::{Expression, Identifier, IfExpression, Node, Statement};
use crate::environment::Environment;
use crate::object::{Array, Boolean, Builtin, Function, Integer, Object, ReturnValue, StringObj};
use std::collections::HashMap;

pub struct Evaluator {
    builtins: HashMap<&'static str, Builtin>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut evaluator = Self {
            builtins: HashMap::new(),
        };
        evaluator.register_builtins();
        evaluator
    }

    fn register_builtins(&mut self) {
        self.builtins.insert(
            "len",
            Builtin {
                func: |args| {
                    if args.len() != 1 {
                        return Object::new_error(format!(
                            "wrong number of arguments, got={}, want=1",
                            args.len()
                        ));
                    }
                    match args[0] {
                        Object::StringObj(ref string) => Object::Integer(Integer {
                            value: string.value.len() as i64,
                        }),
                        _ => Object::new_error(format!(
                            "argument to 'len' not supported, got {}",
                            args[0].get_type()
                        )),
                    }
                },
            },
        );
    }

    fn eval_identifier(&self, node: Identifier, env: Environment) -> Object {
        if let Some(val) = env.get(node.value()) {
            return val;
        }
        if let Some(builtin) = self.builtins.get(node.value()) {
            return Object::Builtin(builtin.clone());
        }
        Object::new_error(format!("identifier not found: {}", node.value()))
    }

    pub fn eval(&self, node: Node, env: Environment) -> Object {
        match node {
            Node::Program(program) => self.eval_program(program.statements, env),
            Node::Statement(statement) => match statement {
                Statement::LetStatement(let_statement) => {
                    let val = self.eval(Node::Expression(let_statement.value), env.clone());
                    if val.is_error() {
                        return val;
                    }
                    env.set(&let_statement.name.token.literal, val)
                }
                Statement::ExpressionStatement(expression_statement) => {
                    self.eval(Node::Expression(expression_statement.expression), env)
                }
                Statement::BlockStatement(block_statement) => {
                    self.eval_block_statement(block_statement.statements, env)
                }
                Statement::ReturnStatement(return_statement) => {
                    let value = self.eval(Node::Expression(return_statement.return_value), env);
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
                Expression::Boolean(boolean) => self.native_bool_to_boolean_object(boolean.value),
                Expression::PrefixExpression(prefix_expression) => {
                    let right = self.eval(Node::Expression(*prefix_expression.right), env);
                    if right.is_error() {
                        right
                    } else {
                        self.eval_prefix_expression(&prefix_expression.operator, right)
                    }
                }
                Expression::InfixExpression(infix_expression) => {
                    let left = self.eval(Node::Expression(*infix_expression.left), env.clone());
                    if left.is_error() {
                        return left;
                    }
                    let right = self.eval(Node::Expression(*infix_expression.right), env);
                    if right.is_error() {
                        return right;
                    }
                    self.eval_infix_expression(&infix_expression.operator, left, right)
                }
                Expression::IfExpression(if_expression) => {
                    self.eval_if_expression(if_expression, env)
                }
                Expression::Identifier(identifier) => self.eval_identifier(identifier, env),
                Expression::FunctionLiteral(function_literal) => Object::Function(Function {
                    parameters: function_literal.parameters,
                    env: env.clone(),
                    body: function_literal.body,
                }),
                Expression::CallExpression(call_expression) => {
                    let func = self.eval(Node::Expression(*call_expression.function), env.clone());
                    if func.is_error() {
                        return func;
                    }
                    let mut args = self.eval_expressions(call_expression.arguments, env);
                    if args.len() == 1 && args[0].is_error() {
                        args.swap_remove(0)
                    } else {
                        self.apply_function(func, args)
                    }
                }
                Expression::StringLiteral(string_literal) => Object::StringObj(StringObj {
                    value: string_literal.token.literal,
                }),
                Expression::ArrayLiteral(array_literal) => {
                    let mut elements = self.eval_expressions(array_literal.elements, env);
                    if elements.len() == 1 && elements[0].is_error() {
                        elements.swap_remove(0)
                    } else {
                        Object::Array(Array { elements })
                    }
                }
                Expression::IndexExpression(index_expression) => {
                    let left = self.eval(Node::Expression(*index_expression.left), env.clone());
                    if left.is_error() {
                        return left;
                    }
                    let index = self.eval(Node::Expression(*index_expression.index), env);
                    if index.is_error() {
                        return index;
                    }
                    return self.eval_index_expression(left, index);
                }
                Expression::Empty() => panic!("you should not be here"),
            },
        }
    }

    fn eval_index_expression(&self, left: Object, index: Object) -> Object {
        if let Object::Array(array) = left {
            if let Object::Integer(index) = index {
                return self.eval_array_index_expression(array, index);
            } else {
                Object::new_error(format!("can't index array with: {}", index.get_type()))
            }
        } else {
            Object::new_error(format!("index operator not supported: {}", left.get_type()))
        }
    }

    fn eval_array_index_expression(&self, array: Array, index: Integer) -> Object {
        if index.value < 0 || index.value as usize >= array.elements.len() {
            Object::new_null()
        } else {
            array.elements[index.value as usize].clone()
        }
    }

    fn apply_function(&self, obj: Object, args: Vec<Object>) -> Object {
        match obj {
            Object::Function(func) => {
                let params = func.parameters;
                let env = func.env;
                let body = func.body;
                let extended_env = self.extend_function_env(params, env, args);
                let evaluated = self.eval(
                    Node::Statement(Statement::BlockStatement(body)),
                    extended_env,
                );
                self.unwrap_return_value(evaluated)
            }
            Object::Builtin(builtin) => (builtin.func)(args),
            _ => Object::new_error(format!("not a function: {}", obj.get_type())),
        }
    }

    fn unwrap_return_value(&self, obj: Object) -> Object {
        match obj {
            Object::ReturnValue(return_value) => *return_value.value,
            _ => obj,
        }
    }

    fn extend_function_env(
        &self,
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

    fn eval_expressions(&self, expressions: Vec<Expression>, env: Environment) -> Vec<Object> {
        let mut result = Vec::new();
        for expression in expressions {
            let evaluated = self.eval(Node::Expression(expression), env.clone());
            if evaluated.is_error() {
                return vec![evaluated];
            }
            result.push(evaluated);
        }
        result
    }

    fn eval_if_expression(&self, if_expression: IfExpression, env: Environment) -> Object {
        let condition = self.eval(Node::Expression(*if_expression.condition), env.clone());
        if condition.is_error() {
            return condition;
        } else if self.is_truthy(condition) {
            self.eval(
                Node::Statement(Statement::BlockStatement(if_expression.consequence)),
                env,
            )
        } else if let Some(alternative) = if_expression.alternative {
            self.eval(Node::Statement(Statement::BlockStatement(alternative)), env)
        } else {
            Object::new_null()
        }
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Null(_) => false,
            Object::Boolean(boolean) => boolean.value,
            _ => true,
        }
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Object::new_error(format!(
                "unknown operator: {}{}",
                operator,
                right.get_type()
            )),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        if let Object::Integer(left_integer) = &left {
            if let Object::Integer(right_integer) = &right {
                return self.eval_integer_infix_expression(operator, left_integer, right_integer);
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
                return self.eval_boolean_infix_expression(operator, left_boolean, right_boolean);
            } else {
                return Object::new_error(format!(
                    "type mismatch: {} {} {}",
                    left.get_type(),
                    operator,
                    right.get_type()
                ));
            }
        } else if let Object::StringObj(left_string_obj) = &left {
            if let Object::StringObj(right_string_obj) = &right {
                return self.eval_string_infix_expression(
                    operator,
                    left_string_obj,
                    right_string_obj,
                );
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

    fn eval_string_infix_expression(
        &self,
        operator: &str,
        left: &StringObj,
        right: &StringObj,
    ) -> Object {
        if operator != "+" {
            Object::new_error(format!("unknown operator: String {operator} String"))
        } else {
            Object::StringObj(StringObj {
                value: format!("{}{}", left.value, right.value),
            })
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: &str,
        left: &Boolean,
        right: &Boolean,
    ) -> Object {
        match operator {
            "==" => self.native_bool_to_boolean_object(left == right),
            "!=" => self.native_bool_to_boolean_object(left != right),
            _ => Object::new_error(format!("unknown operator: Boolean {operator} Boolean")),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &str,
        left: &Integer,
        right: &Integer,
    ) -> Object {
        match operator {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "*" => Object::Integer(left * right),
            "/" => Object::Integer(left / right),
            "<" => self.native_bool_to_boolean_object(left < right),
            ">" => self.native_bool_to_boolean_object(left > right),
            "==" => self.native_bool_to_boolean_object(left == right),
            "!=" => self.native_bool_to_boolean_object(left != right),
            _ => Object::new_error(format!("unknown operator: Integer {operator} Integer")),
        }
    }

    fn native_bool_to_boolean_object(&self, val: bool) -> Object {
        if val {
            Object::new_true()
        } else {
            Object::new_false()
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(integer) => Object::Integer(Integer {
                value: integer.value * -1,
            }),
            _ => Object::new_error(format!("unknown operator: -{}", right.get_type())),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => self.native_bool_to_boolean_object(!boolean.value),
            Object::Null(_) => Object::new_true(),
            _ => Object::new_false(),
        }
    }

    fn eval_program(&self, statements: Vec<Statement>, env: Environment) -> Object {
        let mut result = Object::new_null();
        for statement in statements {
            result = self.eval(Node::Statement(statement), env.clone());
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

    fn eval_block_statement(&self, statements: Vec<Statement>, env: Environment) -> Object {
        let mut result = Object::new_null();
        for statement in statements {
            result = self.eval(Node::Statement(statement), env.clone());
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
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::{
        ast::Node, environment::Environment, lexer::Lexer, object::Object, parser::Parser,
    };

    use super::Evaluator;

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
            test_integer_object(&evaluated, expected);
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
                test_integer_object(&evaluated, expected_int);
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
            test_integer_object(&evaluated, expected);
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
            ("\"Hello\" - \"World\"", "unknown operator: String - String"),
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
            test_integer_object(&test_eval(input), expected);
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
            test_integer_object(&test_eval(input), expected);
        }
    }

    #[test]
    fn test_closures() {
        let input =
            "let a = 1; let newAdder = fn(x) { fn(y) { x + y + a }; }; let addTwo = newAdder(2); addTwo(2);";
        test_integer_object(&test_eval(input), 5);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input);
        let string_obj = match evaluated {
            Object::StringObj(string_obj) => string_obj,
            _ => panic!("not a string_obj"),
        };
        assert_eq!("Hello World!", string_obj.value)
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let evaluated = test_eval(input);
        let string_obj = match evaluated {
            Object::StringObj(string_obj) => string_obj,
            _ => panic!("not a string_obj"),
        };
        assert_eq!("Hello World!", string_obj.value)
    }

    #[test]
    fn test_builtin_functions() {
        let tests: Vec<(&'static str, &'static dyn Any)> = vec![
            (r#"len("")"#, &0),
            (r#"len("four")"#, &4),
            (r#"len("hello world")"#, &11),
            (r#"len(1)"#, &"argument to 'len' not supported, got Integer"),
            (
                r#"len("one", "two")"#,
                &"wrong number of arguments, got=2, want=1",
            ),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            if let Some(integer) = expected.downcast_ref::<i64>() {
                test_integer_object(&evaluated, *integer);
            } else if let Some(string) = expected.downcast_ref::<&str>() {
                match evaluated {
                    Object::Error(error) => assert_eq!(*string, error.message),
                    _ => panic!("not an error object"),
                }
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3];";
        let evaluated = test_eval(input);
        let array = match evaluated {
            Object::Array(array) => array,
            _ => panic!("not an array object"),
        };
        assert_eq!(3, array.elements.len());
        test_integer_object(&array.elements[0], 1);
        test_integer_object(&array.elements[1], 4);
        test_integer_object(&array.elements[2], 6);
    }

    #[test]
    fn test_array_index_expressions() {
        let tests: Vec<(&'static str, Option<i64>)> = vec![
            ("[1, 2, 3][0]", Some(1)),
            ("[1, 2, 3][1]", Some(2)),
            ("[1, 2, 3][2]", Some(3)),
            ("[1, 2, 3][1 + 1];", Some(3)),
            ("let i = 0; [1][i];", Some(1)),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(2),
            ),
            ("[1, 2, 3][3]", None),
            ("[1, 2, 3][-1]", None),
        ];
        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input);
            if let Some(int) = expected {
                test_integer_object(&evaluated, int);
            } else {
                test_null_object(evaluated);
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().expect("to parse program");
        let env = Environment::new();
        let evaluator = Evaluator::new();
        evaluator.eval(Node::Program(program), env)
    }

    fn test_integer_object(obj: &Object, expected: i64) {
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
