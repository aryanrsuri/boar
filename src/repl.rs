// src/repl.rs

use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Stdin, Stdout, Write};

const PROMPT: &str = "kline> ";
const CONTINUE_PROMPT: &str = ".... ";

/// Starts the Read-Eval-Print Loop for the Kline language.
pub fn start(stdin: Stdin, mut stdout: Stdout) -> io::Result<()> {
    println!("Kline REPL v0.1.0");
    println!("Enter code, a blank line to evaluate, or 'exit' to quit.");

    // This buffer will hold the multi-line input until it's ready to be parsed.
    let mut buffer = String::new();

    loop {
        // Choose the appropriate prompt based on whether we are in the middle of
        // a multi-line input block.
        let prompt = if buffer.is_empty() {
            PROMPT
        } else {
            CONTINUE_PROMPT
        };

        // Print the prompt and flush stdout to ensure it appears before input is read.
        write!(stdout, "{}", prompt)?;
        stdout.flush()?;

        let mut line = String::new();
        if stdin.read_line(&mut line)? == 0 {
            // EOF (Ctrl+D) was received, so exit gracefully.
            println!(); // Print a newline to keep the shell prompt clean
            break;
        }

        // 1. Check for exit command (only if it's the start of a new block).
        if buffer.is_empty() && (line.trim() == "exit" || line.trim() == "quit") {
            break;
        }

        // 2. Check for the "end of input" signal (a blank line).
        if line.trim().is_empty() {
            // If the buffer is empty, the user just hit enter at a new prompt.
            // Do nothing and show a new prompt.
            if buffer.is_empty() {
                continue;
            }

            // If the buffer is NOT empty, it's time to parse the code.
            let lexer = Lexer::new(&buffer);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            if !parser.errors.is_empty() {
                print_errors(&mut stdout, &parser.errors)?;
            } else {
                // On success, pretty-print the AST.
                writeln!(stdout, "--- AST ---")?;
                writeln!(stdout, "{:#?}", program)?;
                writeln!(stdout, "-----------")?;
            }

            // Reset the buffer for the next block of code.
            buffer.clear();
        } else {
            // 3. Append the current line to the multi-line buffer.
            buffer.push_str(&line);
        }
    }

    writeln!(stdout, "Goodbye!")?;
    Ok(())
}

/// Helper function to print parser errors in a formatted way.
fn print_errors(stdout: &mut Stdout, errors: &[String]) -> io::Result<()> {
    writeln!(stdout, "--- Parser Errors ---")?;
    writeln!(
        stdout,
        "  The parser found {} error(s):",
        errors.len()
    )?;
    for msg in errors {
        writeln!(stdout, "\t- {}", msg)?;
    }
    writeln!(stdout, "---------------------")?;
    Ok(())
}
