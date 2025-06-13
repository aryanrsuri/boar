// This file defines the Abstract Syntax Tree (AST) for the Kline language.
// The structure is designed to be built by the parser and consumed by later
// compilation stages like a type-checker or interpreter/code generator.

// A helper newtype for identifiers for better type safety and clarity.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Identifier(pub String);

// A path represents a sequence of identifiers, e.g., `std.fmt.println` or `Maybe.Just`.
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    pub segments: Vec<Identifier>,
}

// The root of any Kline program is a `Program` node, which is a list of top-level statements.
#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<TopLevelStatement>,
}

// Top-level statements are the building blocks of a program.
// We differentiate them from regular statements that can appear inside blocks.
#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement {
    Using(UsingStatement),
    Declaration(DeclarationStatement),
    //Comment(String), // Optional: if you want to preserve top-level comments.
}

// Statements that can appear within a block (e.g., function body, if/else).
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Declaration(DeclarationStatement),
    Return(ReturnStatement),
    Expression(Expression), // For expressions used as statements, e.g., `Vc.push(3);`
    // Comments are usually discarded by the parser, but you can add a variant here if needed.
}

// A block of statements, enclosed in `{}`.
#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

// Represents all kinds of declarations.
#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationStatement {
    Binding(BindingDeclaration),
    Function(FunctionDeclaration),
    TypeAlias(TypeAliasDeclaration),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
}

// `using std.fmt`
#[derive(Debug, PartialEq, Clone)]
pub struct UsingStatement {
    pub path: Path,
}

// `let` or `var` bindings.
#[derive(Debug, PartialEq, Clone)]
pub struct BindingDeclaration {
    pub name: Identifier,
    pub type_ann: Option<Type>, // Type annotation is optional for type inference.
    pub value: Expression,
    pub is_mutable: bool, // Differentiates `let` (false) from `var` (true).
}

// `return expression`
#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>, // `return` is equivalent to `return ()`
}

// `fn name<...>(...) -> ... { ... }`
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub generic_params: Option<Vec<Identifier>>,
    pub params: Vec<FunctionParameter>,
    pub return_type: Type,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub type_ann: Type,
}

// `type new_name = old_type`
#[derive(Debug, PartialEq, Clone)]
pub struct TypeAliasDeclaration {
    pub name: Identifier,
    pub generic_params: Option<Vec<Identifier>>,
    pub target_type: Type,
}

// `struct name<...> = { ... }`
#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub generic_params: Option<Vec<Identifier>>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub type_ann: Type,
}

// `enum name<...> = { ... }`
#[derive(Debug, PartialEq, Clone)]
pub struct EnumDeclaration {
    pub name: Identifier,
    pub generic_params: Option<Vec<Identifier>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
    pub name: Identifier,
    pub payload_type: Option<Type>, // e.g., `Just: T`, `Nothing` has None
}

// Represents all possible expressions in the language.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Path(Path), // For `std.fmt` or enum variants like `Maybe.Just`

    // Literals
    Char(char),
    Integer(i64), // Parser should convert from string
    Float(f64),   // Parser should convert from string
    String(String),
    Bool(bool),
    Unit, // For `()`
    GenericIdentifier { base: Identifier, args: Vec<Type> },
    Array(ArrayLiteral),
    Tuple(TupleLiteral),
    Map(MapLiteral),
    Struct(StructLiteral),

    // Operations
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Assignment(AssignmentExpression),
    CompoundAssignment(CompoundAssignmentExpression),
    Index(IndexExpression),       // `array[index]`
    Call(CallExpression),         // `func(arg1, arg2)`
    MemberAccess(MemberAccessExpression), // `obj.field`

    // Control Flow & Grouping
    If(IfExpression),
    For(ForExpression), // The `for` in the example acts like a `while` loop
    Grouped(Box<Expression>), // For `(1 + 2)`
    Cast(CastExpression),   // For `(uint)n`
}

// Represents all possible type annotations.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Identifier(Identifier), // `u32`, `string`, `T`
    Tuple(Vec<Type>),       // `(f32, f32)`
    Array { size: Box<Expression>, element_type: Box<Type> }, // `[3]u32`
    Generic { base: Identifier, args: Vec<Type> }, // `Map<string, u32>`
    Unit, // For `()` return type
}


// --- Expression Details ---

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundAssignmentOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompoundAssignmentExpression {
    pub left: Box<Expression>,
    pub operator: CompoundAssignmentOperator,
    pub right: Box<Expression>,
}


#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleLiteral {
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MapLiteral {
    // e.g. `Map{"hello": 1, "world": 2}`
    pub type_name: Option<Identifier>, // The `Map` part
    pub pairs: Vec<(Expression, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLiteral {
    // e.g. `gpu{...}` or `.{...}`
    pub type_path: Option<Path>, // The `gpu` or inferred type for `.`
    pub fields: Vec<(Identifier, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperator {
    Not,      // !
    Negate,   // -
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOperator {
    Plus, Minus, Multiply, Divide, Modulo,
    Equals, NotEquals, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual,
    // Add others as needed: And, Or, etc.
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentExpression {
    pub left: Box<Expression>, // Can be identifier, index, member access
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>, // Can be identifier, path, member access
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccessExpression {
    pub object: Box<Expression>,
    pub member: Identifier, // e.g. `1` for tuple access or a field name
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForExpression {
    pub condition: Box<Expression>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CastExpression {
    pub target_type: Type,
    pub expression: Box<Expression>,
}
