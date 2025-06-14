// parser.rs

use crate::ast::*;
use crate::lexer::{Lexer, Token};

// Precedence levels for operators, from lowest to highest.
// This is the core of the Pratt parsing algorithm.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Assign,      // =
    Equals,      // ==, !=
    LessGreater, // <, >
    Sum,         // +, -
    Product,     // *, /
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[i], object.field
}

// Maps a token to its precedence level.
fn token_to_precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal | Token::PlusEqual | Token::MinusEqual | Token::AsteriskEqual | Token::SlashEqual => Precedence::Assign,
        Token::EqualEqual | Token::NotEqual => Precedence::Equals,
        Token::LessThan | Token::GreaterThan | Token::LessThanOrEqual | Token::GreaterThanOrEqual => {
            Precedence::LessGreater
        }
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Asterisk | Token::ForwardSlash | Token::Percent => Precedence::Product,
        Token::LeftParen => Precedence::Call,
        Token::LeftBracket => Precedence::Index,
        Token::Period => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

pub struct Parser {
    lexer: Lexer,
    curr: Token,
    peek: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr: Token::Eof,
            peek: Token::Eof,
            errors: Vec::new(),
        };
        // Read two tokens, so curr and peek are both populated
        parser.next_token();
        parser.next_token();
        parser
    }
    
    // --- Public API ---

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.curr != Token::Eof {
            match self.parse_top_level_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(_) => {
                    // An error was already recorded by the parsing function.
                    // We can synchronize to the next statement to continue parsing.
                    self.synchronize();
                }
            }
            self.next_token();
        }
        program
    }

    // --- Core Logic & Helpers ---

    fn next_token(&mut self) {
        self.curr = self.peek.clone();
        // Skip over comments as they are not part of the executable AST
        // You could collect them if you were building a code formatter.
        loop {
            self.peek = self.lexer.lex();
            if !matches!(self.peek, Token::Comment(_)) {
                break;
            }
        }
    }
    
    // Helper to advance if peek is what we expect.
    fn expect_peek(&mut self, expected: Token) -> bool {
        if self.peek == expected {
            self.next_token();
            true
        } else {
            self.peek_error(expected);
            false
        }
    }

    fn peek_error(&mut self, expected: Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, self.peek
        );
        self.errors.push(msg);
    }
    
    // Tries to recover from an error by advancing until a token that
    // likely starts a new statement.
    fn synchronize(&mut self) {
        while self.curr != Token::Eof {
            if self.curr == Token::SemiColon {
                return;
            }
            match self.peek {
                Token::Fn | Token::Let | Token::Var | Token::Struct | Token::Enum | Token::Type => {
                    return;
                }
                _ => self.next_token(),
            }
        }
    }

    // --- Statement Parsers ---

    fn parse_top_level_statement(&mut self) -> Result<TopLevelStatement, ()> {
        match self.curr {
            Token::Using => self.parse_using_statement().map(TopLevelStatement::Using),
            Token::Let | Token::Var => self.parse_binding_declaration().map(|d| TopLevelStatement::Declaration(DeclarationStatement::Binding(d))),
            Token::Fn => self.parse_function_declaration().map(|d| TopLevelStatement::Declaration(DeclarationStatement::Function(d))),
            Token::Type => self.parse_type_alias_declaration().map(|d| TopLevelStatement::Declaration(DeclarationStatement::TypeAlias(d))),
            Token::Struct => self.parse_struct_declaration().map(|d| TopLevelStatement::Declaration(DeclarationStatement::Struct(d))),
            Token::Enum => self.parse_enum_declaration().map(|d| TopLevelStatement::Declaration(DeclarationStatement::Enum(d))),
            _ => {
                let msg = format!("unexpected token at top level: {:?}", self.curr);
                self.errors.push(msg);
                Err(())
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ()> {
        match self.curr {
            Token::Let | Token::Var => self.parse_binding_declaration().map(|d| Statement::Declaration(DeclarationStatement::Binding(d))),
            Token::Return => self.parse_return_statement().map(Statement::Return),
            // Default to an expression statement
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_using_statement(&mut self) -> Result<UsingStatement, ()> {
        self.next_token(); // consume 'using'
        let path = self.parse_path()?;
        if !self.expect_peek(Token::SemiColon) { return Err(()); }
        Ok(UsingStatement { path })
    }

    fn parse_binding_declaration(&mut self) -> Result<BindingDeclaration, ()> {
        let is_mutable = self.curr == Token::Var;
        self.next_token(); // consume 'let' or 'var'

        let name = self.parse_identifier()?;

        let mut type_ann = None;
        if self.peek == Token::Colon {
            self.next_token(); // consume ':'
            self.next_token(); // consume type identifier
            type_ann = Some(self.parse_type()?);
        }

        if !self.expect_peek(Token::Equal) { return Err(()); }
        self.next_token(); // consume '='

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::SemiColon {
            self.next_token();
        }

        Ok(BindingDeclaration { name, type_ann, value, is_mutable })
    }
    
    fn parse_expression_statement(&mut self) -> Result<Statement, ()> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        
        // Expression statements can optionally end with a semicolon
        if self.peek == Token::SemiColon {
            self.next_token();
        }

        Ok(Statement::Expression(expr))
    }
    
    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ()> {
        self.next_token(); // consume 'return'
        
        let value = if self.curr == Token::SemiColon {
            None
        } else {
            Some(self.parse_expression(Precedence::Lowest)?)
        };
        
        if self.peek == Token::SemiColon {
            self.next_token();
        }
        
        Ok(ReturnStatement { value })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ()> {
        let mut statements = Vec::new();
        self.next_token(); // consume '{'

        while self.curr != Token::RightBrace && self.curr != Token::Eof {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        if self.curr != Token::RightBrace {
            self.errors.push("expected '}' to close block".to_string());
            return Err(());
        }

        Ok(BlockStatement { statements })
    }

    // --- Type Definition Parsers ---

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ()> {
        self.next_token(); // consume 'fn'
        let name = self.parse_identifier()?;
        
        let mut generic_params = None;
        if self.peek == Token::LessThan {
            self.next_token(); // consume name, current token is now '<'
            generic_params = Some(self.parse_generic_parameters()?);
        }
        
        if !self.expect_peek(Token::LeftParen) { return Err(()); }
        let params = self.parse_function_parameters()?;
        
        let mut return_type = Type::Unit;
        if self.peek == Token::Colon { // Handle explicit return type syntax '->'
            self.next_token(); // consume ':' -> assuming this for now instead of ->
            self.next_token();
            return_type = self.parse_type()?;
        }
        
        if !self.expect_peek(Token::LeftBrace) { return Err(()); }
        let body = self.parse_block_statement()?;
        
        Ok(FunctionDeclaration { name, generic_params, params, return_type, body })
    }
    
    fn parse_function_parameters(&mut self) -> Result<Vec<FunctionParameter>, ()> {
        let mut params = Vec::new();
        if self.peek == Token::RightParen {
            self.next_token(); // consume ')'
            return Ok(params);
        }
        self.next_token(); // consume '('
        
        let first_param = self.parse_function_parameter()?;
        params.push(first_param);
        
        while self.peek == Token::Comma {
            self.next_token();
            self.next_token();
            let next_param = self.parse_function_parameter()?;
            params.push(next_param);
        }
        
        if !self.expect_peek(Token::RightParen) { return Err(()); }
        Ok(params)
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ()> {
        let name = self.parse_identifier()?;
        if !self.expect_peek(Token::Colon) { return Err(()); }
        self.next_token(); // consume ':'
        let type_ann = self.parse_type()?;
        Ok(FunctionParameter { name, type_ann })
    }
    
    fn parse_type_alias_declaration(&mut self) -> Result<TypeAliasDeclaration, ()> {
        self.next_token(); // consume 'type'
        let name = self.parse_identifier()?;
        let mut generic_params = None;
        if self.peek == Token::LessThan {
            self.next_token(); // consume name, current token is now '<'
            generic_params = Some(self.parse_generic_parameters()?);
        }

        if !self.expect_peek(Token::Equal) { return Err(()); }
        self.next_token();
        let target_type = self.parse_type()?;
        if self.peek == Token::SemiColon { self.next_token(); }
        Ok(TypeAliasDeclaration { name, generic_params, target_type })
    }
    
    fn parse_struct_declaration(&mut self) -> Result<StructDeclaration, ()> {
        self.next_token(); // consume 'struct'
        let name = self.parse_identifier()?;
        let mut generic_params = None;
        if self.peek == Token::LessThan {
            self.next_token(); // consume name, current token is now '<'
            generic_params = Some(self.parse_generic_parameters()?);
        }
        if !self.expect_peek(Token::Equal) { return Err(()); }
        if !self.expect_peek(Token::LeftBrace) { return Err(()); }
        
        let fields = self.parse_delimited_list(Token::LeftBrace, Token::RightBrace, Self::parse_struct_field)?;
        Ok(StructDeclaration { name, generic_params, fields })
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ()> {
        let name = self.parse_identifier()?;
        if !self.expect_peek(Token::Colon) { return Err(()); }
        self.next_token();
        let type_ann = self.parse_type()?;
        Ok(StructField { name, type_ann })
    }

    fn parse_enum_declaration(&mut self) -> Result<EnumDeclaration, ()> {
        self.next_token(); // consume 'enum'
        let name = self.parse_identifier()?;
        let mut generic_params = None;
        if self.peek == Token::LessThan {
            self.next_token(); // consume name, current token is now '<'
            generic_params = Some(self.parse_generic_parameters()?);
        }
        if !self.expect_peek(Token::Equal) { return Err(()); }
        if !self.expect_peek(Token::LeftBrace) { return Err(()); }

        let variants = self.parse_delimited_list(Token::LeftBrace, Token::RightBrace, Self::parse_enum_variant)?;
        Ok(EnumDeclaration { name, generic_params, variants })
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ()> {
        let name = self.parse_identifier()?;
        let payload_type = if self.peek == Token::Colon {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };
        Ok(EnumVariant { name, payload_type })
    }


    // --- Type Parsers ---

    fn parse_type(&mut self) -> Result<Type, ()> {
        match &self.curr {
            Token::Identifier(name) => {
                let base = Identifier(name.clone());
                if self.peek == Token::LessThan {
                    self.next_token(); // consume '<'
                    self.next_token();
                    let args = self.parse_comma_separated(Self::parse_type)?;
                    if !self.expect_peek(Token::GreaterThan) { return Err(()); }
                    Ok(Type::Generic { base, args })
                } else {
                    Ok(Type::Identifier(base))
                }
            }
            Token::LeftParen => {
                self.next_token();
                if self.curr == Token::RightParen {
                    return Ok(Type::Unit);
                }
                let types = self.parse_comma_separated(Self::parse_type)?;
                if !self.expect_peek(Token::RightParen) { return Err(()); }
                Ok(Type::Tuple(types))
            }
            Token::LeftBracket => {
                self.next_token(); // consume '['
                let size = self.parse_expression(Precedence::Lowest)?;
                if !self.expect_peek(Token::RightBracket) { return Err(()); }
                self.next_token();
                let element_type = self.parse_type()?;
                Ok(Type::Array { size: Box::new(size), element_type: Box::new(element_type) })
            }
            Token::Ampersand => {
                self.next_token();
                let inner_type = self.parse_type()?;
                Ok(Type::Reference(Box::new(inner_type)))
            },
            _ => {
                self.errors.push(format!("expected a type, found {:?}", self.curr));
                Err(())
            }
        }
    }

    // --- Expression Parsers (Pratt Engine) ---

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        // Prefix parsing
        let mut left_expr = self.parse_prefix()?;

        // Infix parsing loop
        while self.peek != Token::SemiColon && precedence < token_to_precedence(&self.peek) {
            self.next_token();
            left_expr = self.parse_infix(left_expr)?;
        }
        Ok(left_expr)
    }
    
    fn parse_prefix(&mut self) -> Result<Expression, ()> {
        match &self.curr {
           Token::Identifier(name) => {
                if self.peek == Token::LeftBrace {
                    self.parse_map_literal(Identifier(name.clone()))
                } else {
                    self.parse_identifier_or_path_expression()
                }
            },
            Token::Ampersand => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(Expression::AddressOf(Box::new(expr)))
            },
            Token::Integer(val) => val.parse::<i64>().map(Expression::Integer).map_err(|_| self.errors.push("failed to parse integer".into())),
            Token::Float(val) => val.parse::<f64>().map(Expression::Float).map_err(|_| self.errors.push("failed to parse float".into())),
            Token::String(val) => Ok(Expression::String(val.clone())),
            Token::Bool(val) => Ok(Expression::Bool(*val)),
            Token::Char(c) => Ok(Expression::Char(*c)),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::LeftParen => self.parse_grouped_or_tuple_or_cast_expression(),
            Token::LeftBracket => self.parse_array_literal(),
            Token::LeftBrace | Token::Period => self.parse_struct_literal(),
            Token::If => self.parse_if_expression(),
            Token::For => self.parse_for_expression(),
                     _ => {
                self.errors.push(format!("no prefix parse function for {:?}", self.curr));
                Err(())
            }
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, ()> {
        match &self.curr {
            Token::Plus | Token::Minus | Token::Asterisk | Token::ForwardSlash |
            Token::Percent | Token::EqualEqual | Token::NotEqual | Token::LessThan |
            Token::GreaterThan | Token::LessThanOrEqual | Token::GreaterThanOrEqual => {
                self.parse_infix_expression(left)
            }
            Token::Equal => self.parse_assignment_expression(left),
            Token::PlusEqual | Token::MinusEqual | Token::AsteriskEqual | Token::SlashEqual => {
                self.parse_compound_assignment_expression(left)
            }
            Token::LeftParen => self.parse_call_expression(left),
            Token::LeftBracket => self.parse_index_expression(left),
            Token::Period => self.parse_member_access_expression(left),
            _ => {
                self.errors.push(format!("no infix parse function for {:?}", self.curr));
                Err(())
            }
        }
    }

    // --- Specific Expression Parsers ---
    
    fn parse_identifier(&mut self) -> Result<Identifier, ()> {
        if let Token::Identifier(name) = &self.curr {
            Ok(Identifier(name.clone()))
        } else {
            self.errors.push(format!("expected identifier, got {:?}", self.curr));
            Err(())
        }
    }

    fn parse_path(&mut self) -> Result<Path, ()> {
        let mut segments = vec![self.parse_identifier()?];
        while self.peek == Token::Period {
            self.next_token(); // consume '.'
            self.next_token(); // consume identifier
            segments.push(self.parse_identifier()?);
        }
        Ok(Path { segments })
    }

    fn parse_identifier_or_path_expression(&mut self) -> Result<Expression, ()> {
        let mut segments = vec![self.parse_identifier()?];
        while self.peek == Token::Period {
            // Check for tuple access like `opids.1`
            if let Token::Integer(_) = self.peek {
                 // If we have segments already, it's a member access on a path, handle in infix
                 break;
            }
            self.next_token(); // consume '.'
            self.next_token(); // consume identifier
            segments.push(self.parse_identifier()?);
        }

        if segments.len() > 1 {
            Ok(Expression::Path(Path { segments }))
        } else {
            Ok(Expression::Identifier(segments.pop().unwrap()))
        }
    }
    
    fn parse_prefix_expression(&mut self) -> Result<Expression, ()> {
        let operator = match self.curr {
            Token::Bang => PrefixOperator::Not,
            Token::Minus => PrefixOperator::Negate,
            _ => unreachable!(),
        };
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(PrefixExpression { operator, right: Box::new(right) }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        let operator = match self.curr {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::ForwardSlash => InfixOperator::Divide,
            Token::Percent => InfixOperator::Modulo,
            Token::EqualEqual => InfixOperator::Equals,
            Token::NotEqual => InfixOperator::NotEquals,
            Token::LessThan => InfixOperator::LessThan,
            Token::GreaterThan => InfixOperator::GreaterThan,
            Token::LessThanOrEqual => InfixOperator::LessThanOrEqual,
            Token::GreaterThanOrEqual => InfixOperator::GreaterThanOrEqual,
            _ => unreachable!(),
        };
        let precedence = token_to_precedence(&self.curr);
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::Infix(InfixExpression { left: Box::new(left), operator, right: Box::new(right) }))
    }
    
    fn parse_assignment_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        self.next_token(); // consume '='
        let right = self.parse_expression(Precedence::Assign)?;
        Ok(Expression::Assignment(AssignmentExpression { left: Box::new(left), right: Box::new(right) }))
    }

    fn parse_compound_assignment_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        let operator = match self.curr {
            Token::PlusEqual => CompoundAssignmentOperator::Plus,
            Token::MinusEqual => CompoundAssignmentOperator::Minus,
            Token::AsteriskEqual => CompoundAssignmentOperator::Multiply,
            Token::SlashEqual => CompoundAssignmentOperator::Divide,
            _ => unreachable!(),
        };

        let precedence = token_to_precedence(&self.curr);
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::CompoundAssignment(CompoundAssignmentExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_map_literal(&mut self, type_name: Identifier) -> Result<Expression, ()> {
        if !self.expect_peek(Token::LeftBrace) {
            return Err(());
        }
        self.next_token(); // consume '{'

        let mut pairs = Vec::new();
        if self.curr != Token::RightBrace {
            let (key, value) = self.parse_map_pair()?;
            pairs.push((key, value));
            while self.peek == Token::Comma {
                self.next_token();
                self.next_token();
                if self.curr == Token::RightBrace { break; } // trailing comma
                let (key, value) = self.parse_map_pair()?;
                pairs.push((key, value));
            }
        }

        if !self.expect_peek(Token::RightBrace) {
            return Err(());
        }

        Ok(Expression::Map(MapLiteral {
            type_name: Some(type_name),
            pairs,
        }))
    }
    
    fn parse_map_pair(&mut self) -> Result<(Expression, Expression), ()> {
        let key = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::Colon) {
            return Err(());
        }
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        Ok((key, value))
    }
    
    fn parse_grouped_or_tuple_or_cast_expression(&mut self) -> Result<Expression, ()> {
        // Check for cast: `(type)expr`
        if let Token::Identifier(_) = &self.peek {
            // This is a heuristic. We try to parse a type. If it succeeds and is
            // followed by a ')', we assume it's a cast.
            self.next_token();
            if let Ok(ty) = self.parse_type() {
                if self.peek == Token::RightParen {
                    self.next_token(); // consume ')'
                    self.next_token();
                    let expr = self.parse_expression(Precedence::Prefix)?;
                    return Ok(Expression::Cast(CastExpression {
                        target_type: ty,
                        expression: Box::new(expr),
                    }));
                }
                // If it wasn't a cast, we need to backtrack. This is complex.
                // For this implementation, we'll stick to a simpler path.
                // A more robust parser might need more lookahead or backtracking.
            }
            // If parsing a type failed, rewind. We'll just parse as a grouped expression.
            // This part is tricky in a simple parser. We reset to before we tried the cast.
            // A simpler approach for the language design is to use `expr as Type`.
            // Given the example, we'll assume `(type)expr` is valid.
        }

        // Check for tuple or grouped expression
        self.next_token(); // consume '('
        if self.curr == Token::RightParen {
            return Ok(Expression::Unit);
        }
        
        let mut exprs = self.parse_comma_separated(Self::parse_expression_with_lowest_precedence)?;
        
        if !self.expect_peek(Token::RightParen) { return Err(()); }

        if exprs.len() == 1 {
             Ok(Expression::Grouped(Box::new(exprs.pop().unwrap())))
        } else {
             Ok(Expression::Tuple(TupleLiteral { elements: exprs }))
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ()> {
        self.next_token(); // consume 'if'
        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::LeftBrace) { return Err(()); }
        let consequence = self.parse_block_statement()?;
        
        let mut alternative = None;
        if self.peek == Token::Else {
            self.next_token();
            self.next_token(); // Can be `else {` or `else if {`
            if self.curr == Token::If {
                // `else if` is just sugar for `else { if ... }`
                let else_if_expr = self.parse_if_expression()?;
                let block = BlockStatement { statements: vec![Statement::Expression(else_if_expr)] };
                alternative = Some(block);
            } else if self.curr == Token::LeftBrace {
                 alternative = Some(self.parse_block_statement()?);
            } else {
                return Err(());
            }
        }
        Ok(Expression::If(IfExpression { condition: Box::new(condition), consequence, alternative }))
    }

    fn parse_for_expression(&mut self) -> Result<Expression, ()> {
        self.next_token(); // consume 'for'
        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::LeftBrace) { return Err(()); }
        let body = self.parse_block_statement()?;
        Ok(Expression::For(ForExpression { condition: Box::new(condition), body }))
    }
    
    fn parse_array_literal(&mut self) -> Result<Expression, ()> {
        self.next_token(); // consume '['
        let elements = self.parse_comma_separated(Self::parse_expression_with_lowest_precedence)?;
        if !self.expect_peek(Token::RightBracket) { return Err(()); }
        Ok(Expression::Array(ArrayLiteral { elements }))
    }
    
    fn parse_struct_literal(&mut self) -> Result<Expression, ()> {
        let type_path = if let Token::Identifier(_) = &self.curr {
            let path = self.parse_path()?;
            Some(path)
        } else {
            None // This handles the `.{...}` syntax
        };

        if !self.expect_peek(Token::LeftBrace) { return Err(()); }
        let fields = self.parse_delimited_list(Token::LeftBrace, Token::RightBrace, Self::parse_struct_literal_field)?;
        Ok(Expression::Struct(StructLiteral { type_path, fields }))
    }

    fn parse_struct_literal_field(&mut self) -> Result<(Identifier, Expression), ()> {
        // Syntax is `field: value` or `.field = value`
        if self.curr == Token::Period { self.next_token(); }
        let name = self.parse_identifier()?;
        if self.peek != Token::Equal && self.peek != Token::Colon {
            self.errors.push("expected '=' or ':' in struct field initialization".to_string());
            return Err(());
        }
        self.next_token();
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;
        Ok((name, value))
    }

    /*fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ()> {
        self.next_token(); // consume '('
        let arguments = self.parse_comma_separated(Self::parse_expression_with_lowest_precedence)?;
        if !self.expect_peek(Token::RightParen) { return Err(()); }
        if self.curr != Token::RightParen {
            self.errors.push(format!("expected ')' to close argument list, got {:?}", self.curr));
            return Err(())
        }
        Ok(Expression::Call(CallExpression { function: Box::new(function), arguments }))
    }*/
    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ()> {
        let arguments = self.parse_delimited_list(
            Token::LeftParen, 
            Token::RightParen, 
            Self::parse_expression_with_lowest_precedence
        )?;

        Ok(Expression::Call(CallExpression { 
            function: Box::new(function), 
            arguments 
        }))
    }
    
    fn parse_index_expression(&mut self, object: Expression) -> Result<Expression, ()> {
        self.next_token(); // consume '['
        let index = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RightBracket) { return Err(()); }
        Ok(Expression::Index(IndexExpression { object: Box::new(object), index: Box::new(index) }))
    }

    fn parse_member_access_expression(&mut self, object: Expression) -> Result<Expression, ()> {
        self.next_token(); // consume '.'
        let member = match &self.curr {
            Token::Identifier(s) => Identifier(s.clone()),
            Token::Integer(s) => Identifier(s.clone()), // Handle tuple access like `obj.1`
            _ => {
                self.errors.push(format!("expected identifier or integer for member access, got {:?}", self.curr));
                return Err(());
            }
        };
        Ok(Expression::MemberAccess(MemberAccessExpression { object: Box::new(object), member }))
    }

    // --- Generic Helpers for Lists ---
    
    // A helper to make calling parse_expression from the comma-separated helper easier
    fn parse_expression_with_lowest_precedence(&mut self) -> Result<Expression, ()> {
        self.parse_expression(Precedence::Lowest)
    }

    // Generic function to parse comma-separated lists, e.g., `a, b, c`
    fn parse_comma_separated<T, F>(&mut self, mut parse_fn: F) -> Result<Vec<T>, ()>
    where
        F: FnMut(&mut Self) -> Result<T, ()>,
    {
        let mut items = Vec::new();
        
        // Check if the list is empty
        match self.curr {
            Token::RightParen | Token::RightBracket | Token::GreaterThan => return Ok(items),
            _ => (),
        }

        items.push(parse_fn(self)?);

        while self.peek == Token::Comma {
            self.next_token();
            self.next_token();
            // Handle trailing comma
            match self.curr {
                Token::RightParen | Token::RightBracket | Token::GreaterThan | Token::RightBrace => break,
                _ => (),
            }
            items.push(parse_fn(self)?);
        }

        Ok(items)
    }

    fn parse_generic_parameters(&mut self) -> Result<Vec<Identifier>, ()> {
        // We expect the current token to be '<'
        self.next_token(); // consume '<'

        // Handle empty generics, e.g., `fn foo<>()`
        if self.curr == Token::GreaterThan {
            return Ok(Vec::new());
        }

        let params = self.parse_comma_separated(Self::parse_identifier)?;
        
        // After parsing the list, the current token should be the last identifier,
        // and the peek token should be '>'. `expect_peek` will advance past it.
        if !self.expect_peek(Token::GreaterThan) {
            self.errors.push("expected '>' to close generic parameter list".to_string());
            return Err(());
        }
        
        Ok(params)
    }

    fn parse_delimited_list<T, F>(
        &mut self,
        start_token: Token,
        end_token: Token,
        mut parse_fn: F,
    ) -> Result<Vec<T>, ()>
    where
        F: FnMut(&mut Self) -> Result<T, ()>,
    {
        if self.curr != start_token {
            self.errors.push(format!("expected list to start with {:?}", start_token));
            return Err(());
        }

        let mut items = Vec::new();

        if self.peek == end_token {
            self.next_token(); // consume end_token
            return Ok(items);
        }

        self.next_token(); // consume start_token

        loop {
            items.push(parse_fn(self)?);
            if self.peek != Token::Comma {
                break;
            }
            self.next_token(); // consume item
            self.next_token(); // consume comma
        }
        
        if !self.expect_peek(end_token.clone()) {
             self.errors.push(format!("expected token {:?} to close list", end_token));
             return Err(());
        }
        
        Ok(items)
    }
}
