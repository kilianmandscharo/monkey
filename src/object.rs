use std::ops::{Add, Div, Mul, Sub};

#[derive(Clone, Copy)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Object::Integer(integer) => integer.to_string(),
            Object::Boolean(boolean) => boolean.to_string(),
            Object::Null(null) => null.to_string(),
        };
        write!(f, "{content}")
    }
}

#[derive(Clone, Copy)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl std::fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Add for Integer {
    type Output = Integer;
    fn add(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value + rhs.value,
        }
    }
}

impl Sub for Integer {
    type Output = Integer;
    fn sub(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value - rhs.value,
        }
    }
}

impl Mul for Integer {
    type Output = Integer;
    fn mul(self, rhs: Self) -> Self::Output {
        Integer {
            value: self.value * rhs.value,
        }
    }
}

impl Div for Integer {
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

#[derive(Clone, Copy)]
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

impl Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Clone, Copy)]
pub struct Null {}

impl std::fmt::Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

impl Null {
    fn inspect(&self) -> String {
        "null".to_string()
    }
}
