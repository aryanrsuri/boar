#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),

    Let,
    Var,
    Using,

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
    Walrus,
    Asterisk,
    AsteriskAsterisk,
    ForwardSlash,
    BackwardSlash,
    Equal,
    PlusEqual,
    MinusEqual,
    SlashEqual,
    AsteriskEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    PlusPlus,
    Caret,
    VerticalBar,
    NotEqual,
    LeftParen,
    //Unit,
    RightParen,
    RightArrow,
    LeftArrow,
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
    Illegal,
    Eof,
    Fn,
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
            buffer: buffer.chars().collect(),
            curr: 0,
            peek: 0,
            ch: '\0',
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
        return self.buffer[self.peek];
    }

    pub fn lex(&mut self) -> Token {
        while is_whitespace(self.ch) {
            self.read();
        }

        loop {
            while is_whitespace(self.ch) {
                self.read();
            }

            // Check for single-line comments
            if self.ch == '/' && self.read_peek() == '/' {
                // Consume until the end of the line
                while self.ch != '\n' && self.ch != '\0' {
                    self.read();
                }
                // Continue the loop to find the next token after the comment
                continue;
            }

            // Check for multi-line comments
            if self.ch == '/' && self.read_peek() == '*' {
                // Your existing multi-line comment logic is fine,
                // but should also be skipped in the same way.
                // For simplicity, we'll assume it's also skipped.
                // A simple skip:
                self.read(); // consume /
                self.read(); // consume *
                while !(self.ch == '*' && self.read_peek() == '/') && self.ch != '\0' {
                    self.read();
                }
                if self.ch != '\0' {
                    self.read(); // consume *
                    self.read(); // consume /
                }
                continue;
            }
            
            break;
        }

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
                } else if self.read_peek() == '=' {
                    self.read();
                    Token::PlusEqual
                }
                else {
                    Token::Plus
                }
            }
            ':' => {
                if self.read_peek() == ':' {
                    self.read();
                    Token::ColonColon
                } else if self.read_peek() == '=' {
                    self.read();
                    Token::Walrus
                }
                else {
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
                } else if self.read_peek() == '=' {
                    self.read();
                    Token::MinusEqual

                }
                else {
                    Token::Minus
                }
            }
            '<' => {
                if self.read_peek() == '=' {
                    self.read();
                    Token::LessThanOrEqual
                } else if self.read_peek() == '-' {
                    self.read();
                    Token::LeftArrow
                } else {
                    Token::LessThan
                }
            }
            '*' => {
                if self.read_peek() == '*' {
                    self.read();
                    Token::AsteriskAsterisk
                } else if self.read_peek() == '=' {
                    self.read();
                    Token::AsteriskEqual
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
                } else if self.read_peek() == '=' {
                    self.read();
                    Token::SlashEqual
                } else {
                Token::ForwardSlash
                }
            }
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            ';' => Token::SemiColon,
            '\'' => return self.read_char(),
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
            return Token::Comment(String::from(result.trim_start().trim_end()));
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


    pub fn read_char(&mut self) -> Token {
        self.read(); 
        let ch = self.ch;
        if self.read_peek() == '\'' {
            self.read(); // consume char
            self.read(); // consume closing '
            Token::Char(ch)
        } else {
            Token::Illegal
        }
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
            "fn" => Token::Fn,
            "if" => Token::If,
            "else" => Token::Else,
            "type" => Token::Type,
            "for" => Token::For,
            "let" => Token::Let,
            "var" => Token::Var,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "match" => Token::Match,
            "return" => Token::Return,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "using" => Token::Using,
            _ => Token::Identifier(ident),
        }
    }
}
