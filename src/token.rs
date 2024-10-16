#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Token {
    pub t: TokenType,
    pub literal: String,
}

impl std::default::Default for Token {
    fn default() -> Self {
        Self {
            t: TokenType::Illegal,
            literal: "illegal".to_string(),
        }
    }
}

impl Token {
    pub fn new(t: TokenType, ch: char) -> Self {
        Self {
            t,
            literal: ch.to_string(),
        }
    }

    pub fn from_string(t: TokenType, literal: String) -> Self {
        Self { t, literal }
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}
