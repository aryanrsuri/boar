pub mod parser;
pub mod lexer;
pub mod repl;
pub mod ast;

pub fn main() {
    repl::repl();
}
