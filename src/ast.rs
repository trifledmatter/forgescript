#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Unary(Operator, Box<Expr>),
    Group(Box<Expr>),
    FunctionCall(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),              // array or map indexing
    PropertyAccess(Box<Expr>, String),        // accessing properties of objects/structs
    Assignment(Box<Expr>, Box<Expr>),         // assignment expression
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), // ternary condition (cond ? true_expr : false_expr)
    Lambda(Vec<String>, Box<Expr>),           // lambda expressions
    Array(Vec<Expr>),                         // array literals
    Map(Vec<(Expr, Expr)>),                   // map (key-value) literals
    Cast(Box<Expr>, String),                  // type casting
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,                   // for null values
    Array(Vec<Expr>),       // array literals
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
    Error(Expr),
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
    Module(String, Vec<Box<Stmt>>),                                           // scoping
    Go(Box<Stmt>),                                                            // concurrency
    Schedule(Box<Stmt>, String), // scheduling (statement, interval)
    MacroDefinition(String, Vec<String>, Vec<Box<Stmt>>),
    ForeignFunction(String, String, Vec<(String, String)>, String), // ffi: (language, name, params, return type)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Null,
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct AnnotatedExpr {
    expr: Expr,
    annotations: Vec<Annotation>, // metadata / annotations
}

#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    TypeHint(Type),
    Optimized, // this flag should be enabled if the expression has been optimized
}

impl AnnotatedExpr {
    pub fn new(expr: Expr) -> Self {
        AnnotatedExpr {
            expr,
            annotations: Vec::new(),
        }
    }

    pub fn add_annotation(&mut self, annotation: Annotation) {
        self.annotations.push(annotation);
    }

    pub fn is_optimized(&self) -> bool {
        self.annotations.contains(&Annotation::Optimized)
    }

    pub fn get_type_hint(&self) -> Option<&Type> {
        for annotation in &self.annotations {
            if let Annotation::TypeHint(ref typ) = annotation {
                return Some(typ);
            }
        }
        None
    }
}

pub fn optimize_annotated_expr(annotated_expr: &mut AnnotatedExpr) {
    if annotated_expr.is_optimized() {
        return;
    }

    let optimized_expr = match &annotated_expr.expr {
        // folding for binary addition
        Expr::Binary(left, Operator::Add, right) => {
            if let (Expr::Literal(Literal::Number(l)), Expr::Literal(Literal::Number(r))) =
                (&**left, &**right)
            {
                Expr::Literal(Literal::Number(l + r))
            } else {
                annotated_expr.expr.clone()
            }
        }
        _ => annotated_expr.expr.clone(),
    };

    // Replace the original expression with the optimized one
    annotated_expr.expr = optimized_expr;
    // Mark the expression as optimized
    annotated_expr.add_annotation(Annotation::Optimized);
}

pub trait ExprVisitor<T> {
    fn visit_literal(&mut self, expr: &Literal) -> T;
    fn visit_binary(&mut self, left: &Expr, op: &Operator, right: &Expr) -> T;
    fn visit_unary(&mut self, op: &Operator, expr: &Expr) -> T;
    fn visit_identifier(&mut self, name: &String) -> T;
    fn visit_group(&mut self, expr: &Expr) -> T;
    fn visit_function_call(&mut self, name: &String, args: &[Expr]) -> T;
    fn visit_index(&mut self, expr: &Expr, index: &Expr) -> T;
    fn visit_property_access(&mut self, expr: &Expr, property: &String) -> T;
    fn visit_assignment(&mut self, left: &Expr, right: &Expr) -> T;
    fn visit_ternary(&mut self, cond: &Expr, then_expr: &Expr, else_expr: &Expr) -> T;
    fn visit_lambda(&mut self, params: &[String], body: &Expr) -> T;
    fn visit_array(&mut self, elements: &[Expr]) -> T;
    fn visit_map(&mut self, entries: &[(Expr, Expr)]) -> T;
    fn visit_cast(&mut self, expr: &Expr, typ: &String) -> T;
    fn visit_error(&mut self, msg: &String) -> T;
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Expr) -> T;
    fn visit_print(&mut self, expr: &Expr) -> T;
    fn visit_variable_declaration(&mut self, name: &String, init: &Option<Expr>, is_mut: bool) -> T;
    fn visit_function_declaration(&mut self, name: &String, params: &[(String, String)], return_type: &String, body: &[Box<Stmt>]) -> T;
    fn visit_if(&mut self, cond: &Expr, then_branch: &[Box<Stmt>], else_branch: &Option<Vec<Box<Stmt>>>) -> T;
    fn visit_while(&mut self, cond: &Expr, body: &[Box<Stmt>]) -> T;
    fn visit_for(&mut self, var: &String, start: &Expr, end: &Expr, body: &[Box<Stmt>]) -> T;
    fn visit_for_each(&mut self, var: &String, collection: &Expr, body: &[Box<Stmt>]) -> T;
    fn visit_block(&mut self, stmts: &[Box<Stmt>]) -> T;
    fn visit_return(&mut self, expr: &Option<Expr>) -> T;
    fn visit_break(&mut self) -> T;
    fn visit_continue(&mut self) -> T;
    fn visit_import(&mut self, module: &String) -> T;
    fn visit_type_declaration(&mut self, name: &String, fields: &[(String, String)]) -> T;
    fn visit_class_declaration(&mut self, name: &String, fields: &[(String, String)], methods: &[Box<Stmt>]) -> T;
    fn visit_method_declaration(&mut self, name: &String, params: &[(String, String)], return_type: &String, body: &[Box<Stmt>]) -> T;
    fn visit_module(&mut self, name: &String, stmts: &[Box<Stmt>]) -> T;
    fn visit_go(&mut self, stmt: &Stmt) -> T;
    fn visit_schedule(&mut self, stmt: &Stmt, interval: &String) -> T;
    fn visit_macro_definition(&mut self, name: &String, params: &[String], body: &[Box<Stmt>]) -> T;
    fn visit_foreign_function(&mut self, lang: &String, name: &String, params: &[(String, String)], return_type: &String) -> T;
    fn visit_error(&mut self, expr: &Expr) -> T;
}

pub struct Evaluator; // need to improve this later

impl ExprVisitor<Option<f64>> for Evaluator {
    fn visit_literal(&mut self, expr: &Literal) -> Option<f64> {
        if let Literal::Number(n) = expr {
            Some(*n)
        } else {
            None
        }
    }

    fn visit_binary(&mut self, left: &Expr, op: &Operator, right: &Expr) -> Option<f64> {
        let left_val = left.accept(self)?;
        let right_val = right.accept(self)?;

        match op {
            Operator::Add => Some(left_val + right_val),
            Operator::Subtract => Some(left_val - right_val),
            Operator::Multiply => Some(left_val * right_val),
            Operator::Divide => Some(left_val / right_val),
            _ => None,
        }
    }

    fn visit_unary(&mut self, op: &Operator, expr: &Expr) -> Option<f64> {
        let val = expr.accept(self)?;
        match op {
            Operator::Not => Some(-val),
            _ => None,
        }
    }

    fn visit_identifier(&mut self, _name: &String) -> Option<f64> {
        None // identifier evaluation would involve looking up the variable's value
    }

    fn visit_group(&mut self, expr: &Expr) -> Option<f64> {
        expr.accept(self)
    }

    fn visit_function_call(&mut self, _name: &String, _args: &[Expr]) -> Option<f64> {
        None // function calls would require a function lookup and application
    }

    fn visit_index(&mut self, _expr: &Expr, _index: &Expr) -> Option<f64> {
        None // array or map indexing
    }

    fn visit_property_access(&mut self, _expr: &Expr, _property: &String) -> Option<f64> {
        None // property access would involve looking up the property on an object
    }

    fn visit_assignment(&mut self, _left: &Expr, _right: &Expr) -> Option<f64> {
        None // assignments are statements and don't directly evaluate to a value
    }

    fn visit_ternary(&mut self, _cond: &Expr, _then_expr: &Expr, _else_expr: &Expr) -> Option<f64> {
        None // rernary operation would evaluate the condition and one of the branches
    }

    fn visit_lambda(&mut self, _params: &[String], _body: &Expr) -> Option<f64> {
        None // lambdas would need to be stored and applied later
    }

    fn visit_array(&mut self, _elements: &[Expr]) -> Option<f64> {
        None // arrays are collections of values, not directly evaluated to a number
    }

    fn visit_map(&mut self, _entries: &[(Expr, Expr)]) -> Option<f64> {
        None // maps are collections of key-value pairs, not directly evaluated to a number
    }

    fn visit_cast(&mut self, expr: &Expr, _typ: &String) -> Option<f64> {
        expr.accept(self) // assume that a cast doesn't change the value, just the type
    }

    fn visit_error(&mut self, _msg: &String) -> Option<f64> {
        None // errors don't evaluate to values
    }
}

impl Expr { // visitor for expressions
    pub fn accept<T>(&self, visitor: &mut dyn ExprVisitor<T>) -> T {
        match self {
            Expr::Identifier(name) => visitor.visit_identifier(name),
            Expr::Literal(lit) => visitor.visit_literal(lit),
            Expr::Binary(left, op, right) => visitor.visit_binary(left, op, right),
            Expr::Unary(op, expr) => visitor.visit_unary(op, expr),
            Expr::Group(expr) => visitor.visit_group(expr),
            Expr::FunctionCall(name, args) => visitor.visit_function_call(name, args),
            Expr::Index(expr, index) => visitor.visit_index(expr, index),
            Expr::PropertyAccess(expr, property) => visitor.visit_property_access(expr, property),
            Expr::Assignment(left, right) => visitor.visit_assignment(left, right),
            Expr::Ternary(cond, then_expr, else_expr) => visitor.visit_ternary(cond, then_expr, else_expr),
            Expr::Lambda(params, body) => visitor.visit_lambda(params, body),
            Expr::Array(elements) => visitor.visit_array(elements),
            Expr::Map(entries) => visitor.visit_map(entries),
            Expr::Cast(expr, typ) => visitor.visit_cast(expr, typ),
            Expr::Error(msg) => visitor.visit_error(msg),
        }
    }
}

pub fn optimize_expr(expr: &Expr) -> Expr {
    match expr {
        // there's clearly more to be done with this but for now it's just number folding
        Expr::Binary(left, Operator::Add, right) => {
            if let (Expr::Literal(Literal::Number(l)), Expr::Literal(Literal::Number(r))) = (&**left, &**right) {
                Expr::Literal(Literal::Number(l + r))
            } else {
                expr.clone()
            }
        }
        _ => expr.clone(),
    }
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
            Expr::Error(msg) => write!(f, "Error: {}", msg),
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
            Stmt::Error(expr) => write!(f, "Error: {}", expr),
            Stmt::Expression(expr) => write!(f, "{};", expr),
            Stmt::Print(expr) => write!(f, "print {};", expr),
            Stmt::VariableDeclaration(name, Some(init), true) => {
                write!(f, "mut {} = {};", name, init)
            }
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
            Stmt::ForeignFunction(lang, name, params, return_type) => {
                write!(f, "ffi \"{}\" do\n", lang)?;
                write!(f, "def {}(", name)?;
                for (i, (param, typ)) in params.iter().enumerate() {
                    write!(f, "{}: {}", param, typ)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {} end\nend", return_type)
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
