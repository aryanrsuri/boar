#[derive(Debug, PartialEq)]
pub enum Token {
        Identifier(String),
        TypeDef(String),
        Comment(String),
        String(String),
        Integer(String),
        Float(String),
        Char(char),
        Bool(bool),
        Bang,
        Tilde,
        Percent,
        Colon,
        Period,
        Ampersand,
        Plus,
        Minus,
        Asterisk,
        AsteriskAsterisk,
        ForwardSlash,
        BackwardSlash,
        Equal,
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        PlusPlus,
        Caret,
        VerticalBar,
        NotEqual,
        LeftParen,
        Unit,
        RightParen,
        RightArrow,
        PeriodPeriod,
        ColonColon,
        EqualEqual,
        Comma,
        LeftBrace,
        RightBrace,
        LeftBracket,
        RightBracket,
        QuestionMark,
        SemiColon,
        SemiColonSemiColon,
        Illegal,
        Eof,
        Fn,
        Fun,
        Return,
        If,
        Else,
        Type,
        Struct,
        Enum,
        Match,
        For,
}
pub fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

fn is_numeric(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alphanumeric(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || is_numeric(c) || c == '_'
}

#[derive(Debug)]
pub struct Lexer {
        buffer: Vec<char>,
        curr: usize,
        peek: usize,
        pub ch: char,
}

impl Lexer {
        pub fn new(buffer: &str) -> Lexer {
                let mut lexer = Lexer {
                        buffer : buffer.chars().collect(),
                        curr : 0,
                        peek : 0,
                        ch : '\0',
                };
                lexer.read();
                lexer
        }

        pub fn read(&mut self) {
                if self.peek >= self.buffer.len() {
                        self.ch = '\0'
                } else {
                        self.ch = self.buffer[self.peek]
                }
                self.curr = self.peek;
                self.peek += 1;
        }

        pub fn read_peek(&self) -> char {
            return self.buffer[self.peek]
        }

        pub fn lex(&mut self) -> Token {
            while is_whitespace(self.ch) {self.read();}

             let token: Token = match self.ch {
            '=' => {
                if self.read_peek() == '=' {
                    self.read();
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '!' => {
                if self.read_peek() == '=' {
                    self.read();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '+' => {
                if self.read_peek() == '+' {
                    self.read();
                    Token::PlusPlus
                } else {
                    Token::Plus
                }
            }
            ':' => {
                if self.read_peek() == ':' {
                    self.read();
                    Token::ColonColon
                } else {
                    Token::Colon
                }
            }
            '.' => {
                if self.read_peek() == '.' {
                    self.read();
                    Token::PeriodPeriod
                } else {
                    Token::Period
                }
            }
            '>' => {
                if self.read_peek() == '=' {
                    self.read();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '-' => {
                if self.read_peek() == '>' {
                    self.read();
                    Token::RightArrow
                } else {
                    Token::Minus
                }
            }
            '<' => {
                if self.read_peek() == '=' {
                    self.read();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            '*' => {
                if self.read_peek() == '*' {
                    self.read();
                    Token::AsteriskAsterisk
                } else {
                    Token::Asterisk
                }
            }
            '~' => Token::Tilde,
            '%' => Token::Percent,
            '?' => Token::QuestionMark,
            '&' => Token::Ampersand,
            '^' => Token::Caret,
            '|' => Token::VerticalBar,
            '/' => {
                if self.read_peek() == '*' {
                    self.read();
                    return self.read_comment(true);
                } else if self.read_peek() == '/' {
                    self.read();
                    return self.read_comment(false);
                }
                Token::ForwardSlash
            }
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            ';' => Token::SemiColon,
            '"' => return self.read_string(),
            '0'..='9' => return self.read_number(),
            'a'..='z' | 'A'..='Z' => return self.read_identifier(),
            '\\' => Token::BackwardSlash,
            '\0' => Token::Eof,
            _ => Token::Illegal,
             };
         self.read();
         token
        }

        pub fn read_comment(&mut self, multiline: bool) -> Token {
            self.read();
            let mut result = String::new();
            while self.ch != '\0' {
                if self.ch == '\\' {
                    self.read();
                    match self.ch {
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        'n' => result.push('\n'),
                        _ => result.push(self.ch),
                    }
                } else if self.ch == '*' && self.read_peek() == '/' {
                    self.read();
                    self.read();
                    return Token::Comment(String::from(result.trim_start().trim_end()));
                } else {
                    result.push(self.ch);
                }
                self.read();
            }
            if !multiline {
                return Token::Comment(String::from(result.trim_start().trim_end()))
            }
            Token::Illegal
        }

        pub fn read_number(&mut self) -> Token {
            let current = self.curr;
            while is_numeric(self.ch) {
                self.read();
            }

            if self.ch == '.' && is_numeric(self.read_peek()) {
                self.read();
                while is_numeric(self.ch) {
                    self.read();
                }
                let literal = self.buffer[current..self.curr].iter().collect::<String>();
                return Token::Float(literal);
            }

            Token::Integer(self.buffer[current..self.curr].iter().collect::<String>())
        }

        pub fn read_string(&mut self) -> Token {
            self.read();
            let mut result = String::new();
            while self.ch != '\0' && self.ch != '"' {
                if self.ch == '\\' {
                    self.read();
                    match self.ch {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        _ => result.push(self.ch),
                    }
                } else {
                    result.push(self.ch);
                }
                self.read();
            }

            if self.ch == '"' {
                self.read();
                Token::String(result)
            } else {
                Token::Illegal
            }
        }

        pub fn read_identifier(&mut self) -> Token {
            let current = self.curr;
            loop {
                if is_alphanumeric(self.ch) {
                    self.read();
                } else {
                    break;
                }
            }
            let ident = self.buffer[current..self.curr].iter().collect::<String>();
            match ident.as_str() {
                "fun" => Token::Fun,
                "fn" => Token::Fn,
                "if" => Token::If,
                "else" => Token::Else,
                "type" => Token::Type,
                "struct" => Token::Struct,
                "enum" => Token::Enum,
                "match" => Token::Match,
                "return" => Token::Return,
                "true" => Token::Bool(true),
                "false" => Token::Bool(false),
                _ => Token::Identifier(ident),
            }
        }
}
