// src/ast.rs

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Unary(Operator, Box<Expr>),
    Group(Box<Expr>),
    FunctionCall(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>), // array or map indexing
    PropertyAccess(Box<Expr>, String), // accessing properties of objects/structs
    Assignment(Box<Expr>, Box<Expr>), // assignment expression
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), // ternary condition (cond ? true_expr : false_expr)
    Lambda(Vec<String>, Box<Expr>), // lambda expressions
    Array(Vec<Expr>), // array literals
    Map(Vec<(Expr, Expr)>), // map (key-value) literals
    Cast(Box<Expr>, String), // type casting
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Null, // for null values
    Array(Vec<Expr>), // array literals
    Map(Vec<(Expr, Expr)>), // map (key-value) literals
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Not,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDeclaration(String, Option<Expr>),
    FunctionDeclaration(String, Vec<String>, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    While(Expr, Vec<Stmt>),
    For(String, Expr, Expr, Vec<Stmt>),
    ForEach(String, Expr, Vec<Stmt>),
    Block(Vec<Stmt>),
    Return(Option<Expr>),
    Break,
    Continue,
    Import(String),
    StructDeclaration(String, Vec<(String, String)>),
    ClassDeclaration(String, Vec<(String, String)>, Vec<Stmt>),
    MethodDeclaration(String, Vec<String>, Vec<Stmt>), // for methods within a class
}
