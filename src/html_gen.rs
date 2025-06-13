use crate::ast::*;
use std::fmt::Display;

// A trait to convert any AST node into an HTML representation.
pub trait ToHtml {
    fn to_html(&self) -> String;
}

// Helper for generating a collapsible block.
fn details_block(summary: String, details: String) -> String {
    format!(
        "<details><summary>{}</summary><div class=\"details-content\">{}</div></details>",
        summary, details
    )
}

// Helper for turning a list of items into an HTML list.
fn list_to_html<T: ToHtml>(items: &[T]) -> String {
    if items.is_empty() {
        return "".to_string();
    }
    let list_items = items
        .iter()
        .map(|item| format!("<li>{}</li>", item.to_html()))
        .collect::<String>();
    format!("<ul>{}</ul>", list_items)
}

// --- Primitive Implementations ---

impl ToHtml for Identifier {
    fn to_html(&self) -> String {
        format!("<span class=\"identifier\">{}</span>", self.0)
    }
}

impl ToHtml for Path {
    fn to_html(&self) -> String {
        self.segments
            .iter()
            .map(|s| s.to_html())
            .collect::<Vec<_>>()
            .join("<span class=\"operator\">.</span>")
    }
}

impl<T: ToHtml> ToHtml for Option<T> {
    fn to_html(&self) -> String {
        match self {
            Some(t) => t.to_html(),
            None => "<span class=\"none\">None</span>".to_string(),
        }
    }
}

// --- Main AST Node Implementations ---

impl ToHtml for Program {
    fn to_html(&self) -> String {
        details_block(
            "<span class=\"node-type\">Program</span>".to_string(),
            list_to_html(&self.statements),
        )
    }
}

impl ToHtml for TopLevelStatement {
    fn to_html(&self) -> String {
        match self {
            TopLevelStatement::Using(s) => s.to_html(),
            TopLevelStatement::Declaration(d) => d.to_html(),
        }
    }
}

impl ToHtml for Statement {
     fn to_html(&self) -> String {
        match self {
            Statement::Declaration(d) => d.to_html(),
            Statement::Return(r) => r.to_html(),
            Statement::Expression(e) => details_block(
                "<span class=\"node-type\">ExpressionStatement</span>".to_string(),
                e.to_html()
            ),
        }
    }
}

impl ToHtml for DeclarationStatement {
    fn to_html(&self) -> String {
        match self {
            DeclarationStatement::Binding(b) => b.to_html(),
            DeclarationStatement::Function(f) => f.to_html(),
            DeclarationStatement::TypeAlias(t) => t.to_html(),
            DeclarationStatement::Struct(s) => s.to_html(),
            DeclarationStatement::Enum(e) => e.to_html(),
        }
    }
}

// --- Specific Declaration Implementations ---

impl ToHtml for UsingStatement {
    fn to_html(&self) -> String {
        format!(
            "<span class=\"keyword\">using</span> {}",
            self.path.to_html()
        )
    }
}

impl ToHtml for FunctionDeclaration {
    fn to_html(&self) -> String {
        let signature = format!(
            "<span class=\"keyword\">fn</span> {} {} -> {}",
            self.name.to_html(),
            list_to_html(&self.params),
            self.return_type.to_html()
        );
        let body = details_block(
            "<span class=\"node-type\">Block</span>".to_string(),
            list_to_html(&self.body.statements)
        );
        details_block(signature, body)
    }
}

impl ToHtml for FunctionParameter {
    fn to_html(&self) -> String {
        format!("{}: {}", self.name.to_html(), self.type_ann.to_html())
    }
}

impl ToHtml for EnumDeclaration {
    fn to_html(&self) -> String {
        let signature = format!(
            "<span class=\"keyword\">enum</span> {} =",
            self.name.to_html()
        );
        details_block(signature, list_to_html(&self.variants))
    }
}

impl ToHtml for EnumVariant {
     fn to_html(&self) -> String {
        if let Some(payload) = &self.payload_type {
            format!("{}: {}", self.name.to_html(), payload.to_html())
        } else {
            self.name.to_html()
        }
    }
}

impl ToHtml for StructDeclaration {
    fn to_html(&self) -> String {
        let signature = format!(
            "<span class=\"keyword\">struct</span> {} =",
            self.name.to_html()
        );
        details_block(signature, list_to_html(&self.fields))
    }
}

impl ToHtml for StructField {
    fn to_html(&self) -> String {
        format!("{}: {}", self.name.to_html(), self.type_ann.to_html())
    }
}

impl ToHtml for TypeAliasDeclaration {
    fn to_html(&self) -> String {
        let signature = format!(
            "<span class=\"keyword\">type</span> {} =",
            self.name.to_html()
        );
        details_block(signature, self.target_type.to_html())
    }
}


// --- Expression Implementations ---

impl ToHtml for Expression {
    fn to_html(&self) -> String {
        match self {
            Expression::Identifier(id) => id.to_html(),
            Expression::Path(p) => p.to_html(),
            Expression::GenericIdentifier { base, args } => format!(
                "{}<{}>",
                base.to_html(),
                args.iter().map(ToHtml::to_html).collect::<Vec<_>>().join(", ")
            ),
            Expression::Integer(v) => format!("<span class=\"literal\">{}</span>", v),
            Expression::Float(v) => format!("<span class=\"literal\">{}</span>", v),
            Expression::String(v) => format!("<span class=\"literal\">\"{}\"</span>", v),
            Expression::Bool(v) => format!("<span class=\"literal\">{}</span>", v),
            Expression::Char(v) => format!("<span class=\"literal\">'{}'</span>", v),
            Expression::Unit => "()".to_string(),
            Expression::Infix(expr) => format!(
                "({} {} {})",
                expr.left.to_html(),
                format!("<span class=\"operator\">{}</span>", expr.operator),
                expr.right.to_html()
            ),
            Expression::Call(expr) => details_block(
                format!("Call {}", expr.function.to_html()),
                list_to_html(&expr.arguments),
            ),
            Expression::If(expr) => {
                let summary = format!("<span class=\"keyword\">if</span> {}", expr.condition.to_html());
                let mut content = format!("<h4>Consequence</h4><div class=\"details-content\">{}</div>", list_to_html(&expr.consequence.statements));
                if let Some(alt) = &expr.alternative {
                    content.push_str(&format!("<h4>Alternative</h4><div class=\"details-content\">{}</div>", list_to_html(&alt.statements)));
                }
                details_block(summary, content)
            },
            _ => format!("<span class=\"unhandled\">Unhandled Expr: {:?}</span>", self),
        }
    }
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Multiply => write!(f, "*"),
            InfixOperator::Divide => write!(f, "/"),
            InfixOperator::Equals => write!(f, "=="),
            InfixOperator::NotEquals => write!(f, "!="),
            InfixOperator::LessThan => write!(f, "<"),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::LessThanOrEqual => write!(f, "<="),
            InfixOperator::GreaterThanOrEqual => write!(f, ">="),
            InfixOperator::Modulo => write!(f, "%"),
        }
    }
}


// --- Type Implementations ---

impl ToHtml for Type {
    fn to_html(&self) -> String {
        match self {
            Type::Identifier(id) => format!("<span class=\"type\">{}</span>", id.to_html()),
            Type::Generic { base, args } => format!(
                "<span class=\"type\">{}</span><{}>",
                base.to_html(),
                args.iter().map(ToHtml::to_html).collect::<Vec<_>>().join(", ")
            ),
            Type::Tuple(types) => format!(
                "({})",
                types.iter().map(ToHtml::to_html).collect::<Vec<_>>().join(", ")
            ),
            Type::Array { size, element_type } => format!(
                "[{}] {}",
                size.to_html(),
                element_type.to_html()
            ),
            Type::Unit => "()".to_string(),
        }
    }
}

// --- Other Implementations ---

impl ToHtml for BindingDeclaration {
    fn to_html(&self) -> String {
        let keyword = if self.is_mutable { "var" } else { "let" };
        let summary = format!("<span class=\"keyword\">{}</span> {}: {}", keyword, self.name.to_html(), self.type_ann.to_html());
        details_block(summary, self.value.to_html())
    }
}

impl ToHtml for ReturnStatement {
    fn to_html(&self) -> String {
        details_block("<span class=\"keyword\">return</span>".to_string(), self.value.to_html())
    }
}


// --- The Main Generator Function ---

pub fn generate_html(program: &Program) -> String {
    let body_content = program.to_html();

    format!(r#"
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kline AST</title>
    <style>
        body {{
            background-color: #1e1e1e;
            color: #d4d4d4;
            font-family: 'Consolas', 'Menlo', monospace;
            padding: 20px;
        }}
        details {{
            border-left: 1px solid #444;
            padding-left: 20px;
            margin-left: 15px;
        }}
        summary {{
            cursor: pointer;
            padding: 2px;
            outline: none;
        }}
        summary:hover {{
            background-color: #333;
        }}
        .details-content {{
            padding-top: 5px;
        }}
        ul {{
            list-style-type: none;
            padding-left: 20px;
        }}
        li {{
            margin: 2px 0;
        }}
        .node-type {{
            color: #4ec9b0; /* Teal */
            font-weight: bold;
        }}
        .keyword {{
            color: #c586c0; /* Purple */
        }}
        .identifier {{
            color: #9cdcfe; /* Light Blue */
        }}
        .literal {{
            color: #b5cea8; /* Green */
        }}
        .type {{
            color: #4ec9b0; /* Teal */
        }}
        .operator {{
            color: #d4d4d4;
        }}
        .none {{
            color: #888;
            font-style: italic;
        }}
        .unhandled {{
            color: #f44747; /* Red */
            border: 1px solid #f44747;
            padding: 2px;
        }}
    </style>
</head>
<body>
    {}
</body>
</html>
"#, body_content)
}
