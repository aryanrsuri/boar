use std::env;
use std::fs;
use std::process;

mod ast;
mod html;
mod lexer;
mod parser;
mod repl;

use html::generate_pre_tag_html;
use lexer::Lexer;
use parser::Parser;

fn main() {
    // Collect command-line arguments into a vector of strings.
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => {
            println!("No input file provided. Starting Kline REPL...");
            let stdin = std::io::stdin();
            let stdout = std::io::stdout();
            if let Err(e) = repl::start(stdin, stdout) {
                eprintln!("REPL exited with an error: {}", e);
            }
        }
        2 => {
            let filename = &args[1];
            run_file(filename);
        }
        _ => {
            eprintln!("Usage: kl [filename]");
            process::exit(1); // Exit with an error code
        }
    }
}



fn run_file(path: &str) {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: Could not read file '{}': {}", path, e);
            process::exit(1);
        }
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        eprintln!("--- Parser Errors in '{}' ---", path);
        for msg in parser.errors {
            eprintln!("\t- {}", msg);
        }
        eprintln!("------------------------------------");
        process::exit(1);
    } else {
        let html_output = generate_pre_tag_html(&program);
        let output_path = "ast.html";
        if let Err(e) = fs::write(output_path, html_output) {
            eprintln!("Error writing HTML file: {}", e);
            process::exit(1);
        }
        println!("Successfully generated '{}'. Open it in your browser.", output_path);
    }
}
