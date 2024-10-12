use std::ops::{Add, Div, Mul, Sub};

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

#[derive(Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
    Error(Error),
    Function(Function),
    StringObj(StringObj),
    Builtin(Builtin),
    Array(Array),
}

impl Object {
    pub fn new_true() -> Self {
        Object::Boolean(Boolean { value: true })
    }
    pub fn new_false() -> Self {
        Object::Boolean(Boolean { value: false })
    }
    pub fn new_null() -> Self {
        Object::Null(Null {})
    }
    pub fn new_error(message: String) -> Self {
        Object::Error(Error { message })
    }
    pub fn get_type(&self) -> String {
        match *self {
            Object::Integer(_) => "Integer".to_string(),
            Object::Boolean(_) => "Boolean".to_string(),
            Object::Null(_) => "Null".to_string(),
            Object::ReturnValue(_) => "ReturnValue".to_string(),
            Object::Error(_) => "Error".to_string(),
            Object::Function(_) => "Function".to_string(),
            Object::StringObj(_) => "String".to_string(),
            Object::Builtin(_) => "Builtin".to_string(),
            Object::Array(_) => "Array".to_string(),
        }
    }
    pub fn is_error(&self) -> bool {
        match *self {
            Object::Error(_) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::Null(null) => null.to_string(),
            Object::ReturnValue(return_value) => return_value.to_string(),
            Object::Error(error) => error.to_string(),
            Object::Function(function) => function.to_string(),
            Object::StringObj(string_obj) => string_obj.to_string(),
            Object::Builtin(builtin) => builtin.to_string(),
            Object::Array(array) => array.to_string(),
        };
        write!(f, "{content}")
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {{\n{}\n}}",
            self.parameters
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.to_string()
        )
    }
}

pub type BuiltinFunction = fn(arg: Vec<Object>) -> Object;

#[derive(Clone)]
pub struct Builtin {
    pub func: BuiltinFunction,
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function")
    }
}

#[derive(Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl std::fmt::Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|el| el.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone)]
pub struct StringObj {
    pub value: String,
}

impl std::fmt::Display for StringObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone)]
pub struct Error {
    pub message: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR: {}", self.message)
    }
}

#[derive(Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl std::fmt::Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone)]
pub struct Integer {
    pub value: i64,
}

impl std::fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Add for &Integer {
    type Output = Integer;
    fn add(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value + rhs.value,
        }
    }
}

impl Sub for &Integer {
    type Output = Integer;
    fn sub(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value - rhs.value,
        }
    }
}

impl Mul for &Integer {
    type Output = Integer;
    fn mul(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value * rhs.value,
        }
    }
}

impl Div for &Integer {
    type Output = Integer;
    fn div(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value / rhs.value,
        }
    }
}

impl PartialEq for Integer {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialOrd for Integer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.value.cmp(&other.value))
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub value: bool,
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl PartialEq for Boolean {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Clone)]
pub struct Null {}

impl std::fmt::Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}
