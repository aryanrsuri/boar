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

        fn skip_whitespace(&mut self) {
                while is_whitespace(self.ch) {
                        self.read();
                }
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

        pub fn lex(&mut self) -> Token {
            while is_whitespace(self.ch) {self.read();}
            let token: Token = match self.ch {
                '\0' => Token::Eof,
                _ => Token::Identifier(self.ch.to_string()),
            };
            self.read();
            token
        }
}
