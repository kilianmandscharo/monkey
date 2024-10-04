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

pub struct Boolean {
    pub value: bool,
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

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
