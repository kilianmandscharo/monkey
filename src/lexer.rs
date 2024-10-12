use crate::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            ';' => Token::new(TokenType::Semicolon, self.ch),
            ',' => Token::new(TokenType::Comma, self.ch),
            '(' => Token::new(TokenType::LParen, self.ch),
            ')' => Token::new(TokenType::RParen, self.ch),
            '{' => Token::new(TokenType::LBrace, self.ch),
            '}' => Token::new(TokenType::RBrace, self.ch),
            '[' => Token::new(TokenType::LBracket, self.ch),
            ']' => Token::new(TokenType::RBracket, self.ch),
            '+' => Token::new(TokenType::Plus, self.ch),
            '-' => Token::new(TokenType::Minus, self.ch),
            '*' => Token::new(TokenType::Asterisk, self.ch),
            '/' => Token::new(TokenType::Slash, self.ch),
            '<' => Token::new(TokenType::LT, self.ch),
            '>' => Token::new(TokenType::GT, self.ch),
            '!' => self.match_bang(),
            '=' => self.match_equal(),
            '"' => self.read_string(),
            '\0' => Token::new(TokenType::Eof, self.ch),
            ch => return self.match_char(ch),
        };

        self.read_char();

        tok
    }

    fn read_string(&mut self) -> Token {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        Token::from_string(
            TokenType::String,
            self.input[position..self.position].into_iter().collect(),
        )
    }

    fn match_equal(&mut self) -> Token {
        if self.peek_char() == '=' {
            self.read_char();
            Token::from_string(TokenType::Eq, "==".to_string())
        } else {
            Token::new(TokenType::Assign, self.ch)
        }
    }

    fn match_bang(&mut self) -> Token {
        if self.peek_char() == '=' {
            self.read_char();
            Token::from_string(TokenType::NotEq, "!=".to_string())
        } else {
            Token::new(TokenType::Bang, self.ch)
        }
    }

    fn match_char(&mut self, ch: char) -> Token {
        if self.allowed_in_ident(ch) {
            let literal = self.read_identifier();
            Token {
                t: lookup_ident(&literal),
                literal,
            }
        } else if ch.is_digit(10) {
            Token {
                t: TokenType::Int,
                literal: self.read_number(),
            }
        } else {
            Token::new(TokenType::Illegal, self.ch)
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.allowed_in_ident(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn allowed_in_ident(&self, ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn test_next_token() {
        type Test = (TokenType, &'static str);
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;

            "foobar";
            "foo bar";
            [1, 2];
        "#;
        let tests: Vec<Test> = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "foobar"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "foo bar"),
            (TokenType::Semicolon, ";"),
            (TokenType::LBracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::RBracket, "]"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, "\0"),
        ];

        let mut l = Lexer::new(input);

        for test in tests {
            let tok = l.next_token();
            let (expected_type, expected_literal) = test;
            assert_eq!(tok.t, expected_type);
            assert_eq!(tok.literal, expected_literal);
        }
    }
}
