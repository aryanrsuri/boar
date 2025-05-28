use crate::lexer;
pub fn repl() {
    loop {
        let mut inp = String::new(); 
        if std::io::stdin().read_line(&mut inp).is_err() { println!("[ERR]"); continue;}
        let inp = inp.trim();
        let mut l = lexer::Lexer::new(&inp);
        let mut token: lexer::Token = l.lex();
        while token != lexer::Token::Eof {
            println!("{:?}", token);
            token = l.lex();
        }
    }
}


