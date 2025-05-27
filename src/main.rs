pub mod lexer;

pub fn main() {
    let mut l = lexer::Lexer::new("int x = 45;");
    let mut token: lexer::Token = l.lex();
    while token != lexer::Token::Eof {
        println!("{:?}", token);
        token = l.lex();
    }

}
