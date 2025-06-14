// src/html.rs

use crate::ast::Program;

/// Takes a string slice and escapes characters that have special meaning in HTML.
fn escape_html(s: &str) -> String {
    s.replace('&', "&")
     .replace('<', "<")
     .replace('>', ">")
     .replace('"', "\"")
     .replace('\'', "'")
}

/// Generates a simple HTML page that displays the pretty-printed Debug
/// output of the AST inside a <pre> tag for easy reading.
pub fn generate_pre_tag_html(program: &Program) -> String {
    // Get the pretty-printed debug string using the {:#?} format specifier.
    let debug_string = format!("{:#?}", program);

    // Escape the string to prevent the browser from interpreting any < or >
    // characters in the debug output as HTML tags.
    let escaped_ast_string = escape_html(&debug_string);

    // Format the final HTML document.
    format!(
        r#"
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kline AST (Raw View)</title>
    <style>
        body {{
            background-color: #1e1e1e;
            color: #d4d4d4;
            font-family: 'Consolas', 'Menlo', 'Courier New', monospace;
            font-size: 14px;
            margin: 0;
            padding: 2em;
        }}
        pre {{
            margin: 0;
            white-space: pre-wrap;       /* Allows long lines to wrap */
            word-wrap: break-word;       /* Breaks long words if necessary */
        }}
    </style>
</head>
<body>
    <pre><code>{}</code></pre>
</body>
</html>
"#,
        escaped_ast_string
    )
}
