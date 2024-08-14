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
    Pipe,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDeclaration(String, Option<Expr>, bool), // bool is there to track if 'mut' is used
    FunctionDeclaration(String, Vec<(String, String)>, String, Vec<Box<Stmt>>), // (name, params, return type, body)
    If(Expr, Vec<Box<Stmt>>, Option<Vec<Box<Stmt>>>),
    While(Expr, Vec<Box<Stmt>>),
    For(String, Expr, Expr, Vec<Box<Stmt>>),
    ForEach(String, Expr, Vec<Box<Stmt>>),
    Block(Vec<Box<Stmt>>),
    Return(Option<Expr>),
    Break,
    Continue,
    Import(String),
    TypeDeclaration(String, Vec<(String, String)>),
    ClassDeclaration(String, Vec<(String, String)>, Vec<Box<Stmt>>), // name, fields, methods
    MethodDeclaration(String, Vec<(String, String)>, String, Vec<Box<Stmt>>), // (name, params, return type, body)
    Module(String, Vec<Box<Stmt>>), // scoping
    Go(Box<Stmt>), // concurrency
    Schedule(Box<Stmt>, String), // scheduling (statement, interval)
    MacroDefinition(String, Vec<String>, Vec<Box<Stmt>>),
    ForeignFunction(String, String, Vec<(String, String)>), // ffi: (language, name, params)
}


use std::fmt;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Identifier(name) => write!(f, "{}", name),
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Binary(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expr::Unary(op, expr) => write!(f, "({}{})", op, expr),
            Expr::Group(expr) => write!(f, "({})", expr),
            Expr::FunctionCall(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Index(expr, index) => write!(f, "{}[{}]", expr, index),
            Expr::PropertyAccess(expr, property) => write!(f, "{}.{}", expr, property),
            Expr::Assignment(left, right) => write!(f, "{} = {}", left, right),
            Expr::Ternary(cond, then_expr, else_expr) => {
                write!(f, "{} ? {} : {}", cond, then_expr, else_expr)
            }
            Expr::Lambda(params, body) => {
                write!(f, "|")?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "| {}", body)
            }
            Expr::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    write!(f, "{}", elem)?;
                    if i < elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::Map(entries) => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    write!(f, "{}: {}", key, value)?;
                    if i < entries.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Expr::Cast(expr, typ) => write!(f, "({} as {})", expr, typ),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{};", expr),
            Stmt::Print(expr) => write!(f, "print {};", expr),
            Stmt::VariableDeclaration(name, Some(init), true) => write!(f, "mut {} = {};", name, init),
            Stmt::VariableDeclaration(name, Some(init), false) => write!(f, "{} = {};", name, init),
            Stmt::VariableDeclaration(name, None, true) => write!(f, "mut {};", name),
            Stmt::VariableDeclaration(name, None, false) => write!(f, "{};", name),
            Stmt::FunctionDeclaration(name, params, return_type, body) => {
                write!(f, "def {}(", name)?;
                for (i, (param, typ)) in params.iter().enumerate() {
                    write!(f, "{}: {}", param, typ)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {} do\n", return_type)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::If(cond, then_branch, Some(else_branch)) => {
                write!(f, "if {} do\n", cond)?;
                for stmt in then_branch {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end else do\n")?;
                for stmt in else_branch {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::If(cond, then_branch, None) => {
                write!(f, "if {} do\n", cond)?;
                for stmt in then_branch {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::While(cond, body) => {
                write!(f, "while {} do\n", cond)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::For(var, start, end, body) => {
                write!(f, "for {} in {}..{} do\n", var, start, end)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::ForEach(var, collection, body) => {
                write!(f, "foreach {} in {} do\n", var, collection)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::Block(stmts) => {
                write!(f, "do\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::Return(Some(expr)) => write!(f, "return {};", expr),
            Stmt::Return(None) => write!(f, "return;"),
            Stmt::Break => write!(f, "break;"),
            Stmt::Continue => write!(f, "continue;"),
            Stmt::Import(module) => write!(f, "import \"{}\";", module),
            Stmt::TypeDeclaration(name, fields) => {
                write!(f, "type {} do\n", name)?;
                for (field_name, field_type) in fields {
                    write!(f, "{}: {},\n", field_name, field_type)?;
                }
                write!(f, "end")
            }
            Stmt::ClassDeclaration(name, fields, methods) => {
                write!(f, "class {} do\n", name)?;
                for (field_name, field_type) in fields {
                    write!(f, "{}: {},\n", field_name, field_type)?;
                }
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "end")
            }
            Stmt::MethodDeclaration(name, params, return_type, body) => {
                write!(f, "def {}(", name)?;
                for (i, (param, typ)) in params.iter().enumerate() {
                    write!(f, "{}: {}", param, typ)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {} do\n", return_type)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::Module(name, stmts) => {
                write!(f, "module {} do\n", name)?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::Go(stmt) => {
                write!(f, "go do\n{}\nend", stmt)
            }
            Stmt::Schedule(stmt, interval) => {
                write!(f, "schedule {} do\n{}\nend", interval, stmt)
            }
            Stmt::MacroDefinition(name, params, body) => {
                write!(f, "macro {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") do\n")?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "end")
            }
            Stmt::ForeignFunction(lang, name, params) => {
                write!(f, "ffi \"{}\" do\n", lang)?;
                write!(f, "def {}(", name)?;
                for (i, (param, typ)) in params.iter().enumerate() {
                    write!(f, "{}: {}", param, typ)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") end\nend")
            }
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
            Literal::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    write!(f, "{}", elem)?;
                    if i < elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Literal::Map(entries) => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    write!(f, "{}: {}", key, value)?;
                    if i < entries.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulo => "%",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::Less => "<",
            Operator::LessEqual => "<=",
            Operator::Greater => ">",
            Operator::GreaterEqual => ">=",
            Operator::And => "&&",
            Operator::Or => "||",
            Operator::Not => "!",
            Operator::BitwiseAnd => "&",
            Operator::BitwiseOr => "|",
            Operator::BitwiseXor => "^",
            Operator::BitwiseNot => "~",
            Operator::LeftShift => "<<",
            Operator::RightShift => ">>",
            Operator::Pipe => "|>",
        };
        write!(f, "{}", op)
    }
}
