// src/main.rs

use std::env;
use std::fs;
use std::process;

mod html_gen;
mod ast;
mod lexer;
mod parser;
mod repl;

use lexer::Lexer;
use parser::Parser;
use html_gen::generate_html;

fn main() {
    // Collect command-line arguments into a vector of strings.
    let args: Vec<String> = env::args().collect();

    // The first argument (args[0]) is always the path to the executable.
    // We check the number of arguments to decide what to do.
    match args.len() {
        // Case 1: No filename provided, e.g., `cargo run` or `./kl`
        1 => {
            println!("No input file provided. Starting Kline REPL...");
            let stdin = std::io::stdin();
            let stdout = std::io::stdout();
            if let Err(e) = repl::start(stdin, stdout) {
                eprintln!("REPL exited with an error: {}", e);
            }
        }
        // Case 2: A filename is provided, e.g., `cargo run -- example.kl` or `./kl example.kl`
        2 => {
            let filename = &args[1];
            run_file(filename);
        }
        // Case 3: Too many arguments.
        _ => {
            eprintln!("Usage: kl [filename]");
            process::exit(1); // Exit with an error code
        }
    }
}

fn run_file(path: &str) {
    // ... (reading the file is the same)
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
        // --- THIS IS THE NEW PART ---
        println!("Parsing successful. Generating AST visualization...");
        
        // Generate the HTML content
        let html_output = generate_html(&program);
        
        // Write the HTML to a file
        let output_path = "ast.html";
        if let Err(e) = fs::write(output_path, html_output) {
            eprintln!("Error writing HTML file: {}", e);
            process::exit(1);
        }
        
        println!("Successfully generated '{}'. Open it in your browser.", output_path);
    }
}
