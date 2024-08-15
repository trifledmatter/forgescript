// parser.rs
use crate::ast::{Expr, Literal, Operator, Stmt};
use crate::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    // expects expressions, usually starting with the lowest precedence operator
    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    // expects "module" keyword, module name, "do", declarations, "end"
    fn module_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect module name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' after module name.")?;

        let mut statements = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            statements.push(Box::new(self.declaration()?));
        }

        self.consume(TokenType::Keyword, "Expect 'end' after module declaration.")?;

        Ok(Stmt::Module(name_lexeme, statements))
    }

    // expects "def" keyword, function name, '(', parameter list, ')', '->', return type, "do", function body, "end"
    fn function_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect function name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after function name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != ")" {
            loop {
                let param_name = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param_name_lexeme = param_name.lexeme.clone();

                self.consume(TokenType::Punctuation, "Expect ':' after parameter name.")?;

                let param_type = self.consume(TokenType::Identifier, "Expect parameter type.")?;
                parameters.push((param_name_lexeme, param_type.lexeme.clone()));

                if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != "," {
                    break;
                }
                self.advance(); // Advance past the comma
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")
            .and_then(|t| {
                if t.lexeme == ")" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ')' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        self.consume(TokenType::Punctuation, "Expect '->' before return type.")
            .and_then(|t| {
                if t.lexeme == "->" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected '->' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;

        let return_type = self.consume(TokenType::Identifier, "Expect return type.")?;
        let return_type_lexeme = return_type.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' before function body.")?;

        let body = self.block()?;

        Ok(Stmt::FunctionDeclaration(
            name_lexeme,
            parameters,
            return_type_lexeme,
            body,
        ))
    }

    // expects "class" keyword, class name, "do", field declarations and/or methods, "end"
    fn class_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect class name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' after class name.")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            if self.check(TokenType::Identifier) {
                let field_name = self
                    .consume(TokenType::Identifier, "Expect field name.")?
                    .lexeme
                    .clone();

                self.consume(TokenType::Punctuation, "Expect ':' after field name.")?;

                let field_type = self.consume(TokenType::Identifier, "Expect field type.")?;
                fields.push((field_name, field_type.lexeme.clone()));
            } else if self.check(TokenType::Keyword) && self.peek().unwrap().lexeme == "def" {
                self.advance();
                methods.push(Box::new(self.function_declaration()?));
            } else {
                let peeked_token = self.peek().unwrap();
                return Err(format!(
                    "Unexpected token {} at line {}, column {}",
                    peeked_token.lexeme, peeked_token.line, peeked_token.column
                ));
            }
        }

        self.consume(TokenType::Keyword, "Expect 'end' after class declaration.")?;

        Ok(Stmt::ClassDeclaration(name_lexeme, fields, methods))
    }

    // expects "type" keyword, type name, "do", field declarations, "end"
    fn type_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect type name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' after type name.")?;

        let mut fields = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            let field_name = self.consume(TokenType::Identifier, "Expect field name.")?;
            let field_name_lexeme = field_name.lexeme.clone();

            self.consume(TokenType::Punctuation, "Expect ':' after field name.")?;

            let field_type = self.consume(TokenType::Identifier, "Expect field type.")?;
            fields.push((field_name_lexeme, field_type.lexeme.clone()));

            if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != "," {
                break;
            }
            self.advance(); // Advance past the comma
        }

        self.consume(TokenType::Keyword, "Expect 'end' after type declaration.")?;

        Ok(Stmt::TypeDeclaration(name_lexeme, fields))
    }

    // expects "if" keyword, condition expression, "do", then block, optionally "else" with "do" and else block, "end"
    fn if_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")
            .and_then(|t| {
                if t.lexeme == "do" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected 'do' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        let then_branch = self.block()?;
        let mut else_branch = None;

        if self.check(TokenType::Keyword) && self.peek().unwrap().lexeme == "else" {
            self.advance(); // Advance past 'else'
            self.consume(TokenType::Keyword, "Expect 'do' after 'else'.")
                .and_then(|t| {
                    if t.lexeme == "do" {
                        Ok(t)
                    } else {
                        Err(format!(
                            "Expected 'do' but found '{}' at line {}, column {}",
                            t.lexeme, t.line, t.column
                        ))
                    }
                })?;
            else_branch = Some(self.block()?);
        }

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    // expects "while" keyword, condition expression, "do", body block, "end"
    fn while_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")
            .and_then(|t| {
                if t.lexeme == "do" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected 'do' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        let body = self.block()?;
        Ok(Stmt::While(condition, body))
    }

    // expects "for" keyword, loop variable, "in", start expression, "..", end expression, "do", body block, "end"
    fn for_statement(&mut self) -> Result<Stmt, String> {
        let variable = self.consume(TokenType::Identifier, "Expect variable name.")?;
        let variable_lexeme = variable.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'in' after variable.")?;

        let start = self.expression()?;

        self.consume(TokenType::Punctuation, "Expect '..' after start value.")
            .and_then(|t| {
                if t.lexeme == ".." {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected '..' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;

        let end = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after range.")
            .and_then(|t| {
                if t.lexeme == "do" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected 'do' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        let body = self.block()?;

        Ok(Stmt::For(variable_lexeme, start, end, body))
    }

    // expects "def" keyword, macro name, '(', parameter list, ')', "do", macro body, "end"
    fn macro_definition(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect macro name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after macro name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != ")" {
            loop {
                let param = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(param.lexeme.clone());
                if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != "," {
                    break;
                }
                self.advance(); // Advance past the comma
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")
            .and_then(|t| {
                if t.lexeme == ")" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ')' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        self.consume(TokenType::Keyword, "Expect 'do' before macro body.")?;

        let body = self.block()?;

        Ok(Stmt::MacroDefinition(name_lexeme, parameters, body))
    }

    // expects "ffi" keyword, language string, "do", function declaration, "end"
    fn foreign_function(&mut self) -> Result<Stmt, String> {
        let language = self.consume(
            TokenType::StringLiteral,
            "Expect language identifier for FFI.",
        )?;
        let language_lexeme = language.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' after FFI language.")?;

        let name = self.consume(TokenType::Identifier, "Expect function name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after function name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != ")" {
            loop {
                let param_name = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param_name_lexeme = param_name.lexeme.clone();

                self.consume(TokenType::Punctuation, "Expect ':' after parameter name.")?;

                let param_type = self.consume(TokenType::Identifier, "Expect parameter type.")?;
                let param_type_lexeme = param_type.lexeme.clone();

                parameters.push((param_name_lexeme, param_type_lexeme));

                if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != "," {
                    break;
                }
                self.advance(); // Advance past the comma
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")
            .and_then(|t| {
                if t.lexeme == ")" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ')' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        self.consume(
            TokenType::Keyword,
            "Expect 'end' after FFI function declaration.",
        )?;

        Ok(Stmt::ForeignFunction(
            language_lexeme,
            name_lexeme,
            parameters,
        ))
    }

    // expects "import" keyword, module name, ";"
    fn import_statement(&mut self) -> Result<Stmt, String> {
        let module_name =
            self.consume(TokenType::StringLiteral, "Expect module name to import.")?;
        let module_name_lexeme = module_name.lexeme.clone();
        self.consume(TokenType::Punctuation, "Expect ';' after import statement.")
            .and_then(|t| {
                if t.lexeme == ";" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ';' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        Ok(Stmt::Import(module_name_lexeme))
    }

    // expects "mut" keyword (optional), variable name, "=", initializer expression, ";"
    fn variable_declaration(&mut self, mutable: bool) -> Result<Stmt, String> {
        let name_token = self.consume(TokenType::Identifier, "Expect variable name.")?;
        let name = name_token.lexeme.clone();

        let mut initializer = None;
        if self.check(TokenType::Operator) && self.peek().unwrap().lexeme == "=" {
            self.advance();
            initializer = Some(self.expression()?);
        }

        self.consume(
            TokenType::Punctuation,
            "Expect ';' after variable declaration.",
        )
        .and_then(|t| {
            if t.lexeme == ";" {
                Ok(t)
            } else {
                Err(format!(
                    "Expected ';' but found '{}' at line {}, column {}",
                    t.lexeme, t.line, t.column
                ))
            }
        })?;
        Ok(Stmt::VariableDeclaration(name, initializer, mutable))
    }

    // expects "print" keyword, expression, ";"
    fn print_statement(&mut self) -> Result<Stmt, String> {
        println!("Print statement peek before: {:#?}", self.peek());
        let value = self.expression()?;
        println!("Print statement peek after: {:#?}", self.peek());

        self.consume(TokenType::Punctuation, "Expect ';' after value.")
            .and_then(|t| {
                if t.lexeme == ";" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ';' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        Ok(Stmt::Print(value))
    }

    // expects "return" keyword, optional expression, ";"
    fn return_statement(&mut self) -> Result<Stmt, String> {
        let expr = if self.check(TokenType::Punctuation) && self.peek().unwrap().lexeme == ";" {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Punctuation, "Expect ';' after return value.")
            .and_then(|t| {
                if t.lexeme == ";" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ';' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        Ok(Stmt::Return(expr))
    }

    // expects "go" keyword, statement
    fn go_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        Ok(Stmt::Go(Box::new(stmt)))
    }

    // expects "schedule" keyword, statement, "every:", interval string
    fn schedule_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        let interval = self.consume(
            TokenType::StringLiteral,
            "Expect string interval after schedule.",
        )?;
        Ok(Stmt::Schedule(Box::new(stmt), interval.lexeme.clone()))
    }

    // expects expression, ";"
    fn expression_statement(&mut self) -> Result<Stmt, String> {
        println!("Before expr: {:#?}", self.peek());

        let expr = self.expression()?;

        println!("After expr: {:#?}", self.peek());

        if self.peek().map(|t| t.token_type) != Some(TokenType::Punctuation) {
            return Err(format!(
                "Expected ';' after expression but found {:?} at line {}, column {}",
                self.peek().unwrap().token_type, // Current token
                self.peek().unwrap().line,
                self.peek().unwrap().column,
            ));
        }

        self.consume(TokenType::Punctuation, "Expect ';' after expression.")?; // Now consume the semicolon
        Ok(Stmt::Expression(expr))
    }

    // expects assignment expression, which may include a variable or property access, followed by '=' and a value expression
    fn assignment(&mut self) -> Result<Expr, String> {
        println!("Before or expr: {:#?}", self.peek());

        let expr = self.or()?;

        println!("After or expr: {:#?}", self.peek());

        if self.check(TokenType::Operator) && self.peek().unwrap().lexeme == "=" {
            self.advance(); // Advance past the '='
            let equals = self.previous().clone();
            let value = self.assignment()?;
            match expr {
                Expr::Identifier(name) => {
                    return Ok(Expr::Assignment(
                        Box::new(Expr::Identifier(name)),
                        Box::new(value),
                    ));
                }
                Expr::PropertyAccess(object, property) => {
                    return Ok(Expr::Assignment(
                        Box::new(Expr::PropertyAccess(object, property)),
                        Box::new(value),
                    ));
                }
                _ => {
                    return Err(format!(
                        "Invalid assignment target at line {}, column {}",
                        equals.line, equals.column
                    ));
                }
            }
        }

        Ok(expr)
    }

    // expects logical OR expression, which may include '||' operators
    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.check(TokenType::Operator) && self.peek().unwrap().lexeme == "||" {
            self.advance(); // Advance past the '||'
            let operator = Operator::Or;
            let right = self.and()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects logical AND expression, which may include '&&' operators
    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.check(TokenType::Operator) && self.peek().unwrap().lexeme == "&&" {
            self.advance(); // Advance past the '&&'
            let operator = Operator::And;
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects equality expression, which may include '==' or '!=' operators
    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.check(TokenType::Operator)
            && (self.peek().unwrap().lexeme == "==" || self.peek().unwrap().lexeme == "!=")
        {
            let operator = match self.peek().unwrap().lexeme.as_str() {
                "==" => Operator::Equal,
                "!=" => Operator::NotEqual,
                _ => unreachable!(),
            };
            self.advance(); // Advance past '==' or '!='
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects comparison expression, which may include '<', '<=', '>', '>=' operators
    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.check(TokenType::Operator)
            && (self.peek().unwrap().lexeme == "<"
                || self.peek().unwrap().lexeme == "<="
                || self.peek().unwrap().lexeme == ">"
                || self.peek().unwrap().lexeme == ">=")
        {
            let operator = match self.peek().unwrap().lexeme.as_str() {
                "<" => Operator::Less,
                "<=" => Operator::LessEqual,
                ">" => Operator::Greater,
                ">=" => Operator::GreaterEqual,
                _ => unreachable!(),
            };
            self.advance(); // Advance past the comparison operator
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects term expression, which may include '+' or '-' operators
    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.check(TokenType::Operator)
            && (self.peek().unwrap().lexeme == "+" || self.peek().unwrap().lexeme == "-")
        {
            let operator = match self.peek().unwrap().lexeme.as_str() {
                "+" => Operator::Add,
                "-" => Operator::Subtract,
                _ => unreachable!(),
            };
            self.advance(); // Advance past the '+' or '-'
            let right = self.factor()?; // Handle operator precedence correctly
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects factor expression, which may include '*' or '/' operators
    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.check(TokenType::Operator)
            && (self.peek().unwrap().lexeme == "*" || self.peek().unwrap().lexeme == "/")
        {
            let operator = match self.peek().unwrap().lexeme.as_str() {
                "*" => Operator::Multiply,
                "/" => Operator::Divide,
                _ => unreachable!(),
            };
            self.advance(); // Advance past the '*' or '/'
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    // expects unary expression, which may include '!' or '-' operators
    fn unary(&mut self) -> Result<Expr, String> {
        if self.check(TokenType::Operator)
            && (self.peek().unwrap().lexeme == "!" || self.peek().unwrap().lexeme == "-")
        {
            let operator = match self.peek().unwrap().lexeme.as_str() {
                "!" => Operator::Not,
                "-" => Operator::Subtract,
                _ => unreachable!(),
            };
            self.advance(); // Advance past the '!' or '-'
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.call()
    }

    // expects call expression, which may include function calls, property access, or index access
    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        while self.check(TokenType::Punctuation) {
            let previous_token = self.peek().unwrap().clone();
            match previous_token.lexeme.as_str() {
                "(" => {
                    self.advance();
                    expr = self.finish_call(expr)?
                }
                "." => {
                    self.advance();
                    expr = self.property_access(expr)?
                }
                "[" => {
                    self.advance();
                    expr = self.index_access(expr)?
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // expects function call arguments, followed by ')'
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != ")" {
            loop {
                arguments.push(self.expression()?);
                if !self.check(TokenType::Punctuation) || self.peek().unwrap().lexeme != "," {
                    break;
                }
                self.advance(); // Advance past the comma
            }
        }
        self.consume(TokenType::Punctuation, "Expect ')' after arguments.")
            .and_then(|t| {
                if t.lexeme == ")" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ')' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        Ok(Expr::FunctionCall(callee.to_string(), arguments))
    }

    // expects '.' followed by property name
    fn property_access(&mut self, object: Expr) -> Result<Expr, String> {
        let name = self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
        Ok(Expr::PropertyAccess(Box::new(object), name.lexeme.clone()))
    }

    // expects '[' followed by index expression and ']'
    fn index_access(&mut self, object: Expr) -> Result<Expr, String> {
        let index = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ']' after index.")
            .and_then(|t| {
                if t.lexeme == "]" {
                    Ok(t)
                } else {
                    Err(format!(
                        "Expected ']' but found '{}' at line {}, column {}",
                        t.lexeme, t.line, t.column
                    ))
                }
            })?;
        Ok(Expr::Index(Box::new(object), Box::new(index)))
    }

    // expects literals, identifiers, or grouped expressions
    fn primary(&mut self) -> Result<Expr, String> {
        if self.check(TokenType::Boolean) {
            return Ok(Expr::Literal(Literal::Boolean(
                self.advance().lexeme == "true",
            )));
        }

        if self.check(TokenType::Number) {
            return Ok(Expr::Literal(Literal::Number(
                self.advance().lexeme.parse::<f64>().unwrap(),
            )));
        }

        if self.check(TokenType::StringLiteral) {
            return Ok(Expr::Literal(Literal::String(
                self.advance().lexeme.clone(),
            )));
        }

        if self.check(TokenType::Null) {
            return Ok(Expr::Literal(Literal::Null));
        }

        if self.check(TokenType::Identifier) {
            return Ok(Expr::Identifier(self.advance().lexeme.clone()));
        }

        if self.check(TokenType::Punctuation) && self.peek().unwrap().lexeme == "(" {
            self.advance(); // Advance past '('
            let expr = self.expression()?;
            self.consume(TokenType::Punctuation, "Expect ')' after expression.")
                .and_then(|t| {
                    if t.lexeme == ")" {
                        Ok(t)
                    } else {
                        Err(format!(
                            "Expected ')' but found '{}' at line {}, column {}",
                            t.lexeme, t.line, t.column
                        ))
                    }
                })?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        Err(format!(
            "Unexpected token {} at line {}, column {}",
            self.peek().unwrap().lexeme,
            self.peek().unwrap().line,
            self.peek().unwrap().column
        ))
    }

    // expects a block of statements, ending with "end"
    fn block(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut statements = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            statements.push(Box::new(self.statement()?));
        }
        self.consume(TokenType::Keyword, "Expect 'end' after block.")?;
        Ok(statements)
    }

    // expects any kind of statement, either declaration or expression
    fn declaration(&mut self) -> Result<Stmt, String> {
        println!("Declaration token peek test: {:#?}", self.peek());

        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Keyword {
                let keyword = token.lexeme.as_str();
                match keyword {
                    "def" => {
                        self.advance();
                        return self.function_declaration();
                    }
                    "type" => {
                        self.advance();
                        return self.type_declaration();
                    }
                    "class" => {
                        self.advance();
                        return self.class_declaration();
                    }
                    "module" => {
                        self.advance();
                        return self.module_declaration();
                    }
                    "macro" => {
                        self.advance();
                        return self.macro_definition();
                    }
                    "ffi" => {
                        self.advance();
                        return self.foreign_function();
                    }
                    "import" => {
                        self.advance();
                        return self.import_statement();
                    }
                    "mut" => {
                        self.advance();
                        return self.variable_declaration(true);
                    }
                    _ => (),
                }
            }
        }

        self.statement()
    }

    // expects any kind of expression statement
    fn statement(&mut self) -> Result<Stmt, String> {
        println!("Statement token peek test: {:#?}", self.peek());

        if let Some(token) = self.peek() {
            if token.token_type == TokenType::Keyword {
                let keyword = token.lexeme.as_str();

                match keyword {
                    "print" => {
                        self.advance();
                        return self.print_statement();
                    }
                    "if" => {
                        self.advance();
                        return self.if_statement();
                    }
                    "while" => {
                        self.advance();
                        return self.while_statement();
                    }
                    "for" => {
                        self.advance();
                        return self.for_statement();
                    }
                    "return" => {
                        self.advance();
                        return self.return_statement();
                    }
                    "break" => {
                        self.advance();
                        return Ok(Stmt::Break);
                    }
                    "continue" => {
                        self.advance();
                        return Ok(Stmt::Continue);
                    }
                    "go" => {
                        self.advance();
                        return self.go_statement();
                    }
                    "schedule" => {
                        self.advance();
                        return self.schedule_statement();
                    }
                    _ => (),
                }
            }
        }

        self.expression_statement()
    }

    // checks if the current token is of the given type
    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().unwrap().token_type == token_type
    }

    // advances to the next token and returns the previous token
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // checks if the current token is the end of file
    fn is_at_end(&self) -> bool {
        self.peek().unwrap().token_type == TokenType::Eof
    }

    // returns the current token without advancing
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    // returns the previous token
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    // checks if the next token is of the given type and advances if true, otherwise returns an error
    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(format!(
            "{} at line {}, column {}",
            message,
            self.peek().unwrap().line,
            self.peek().unwrap().column
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Literal, Stmt};
    use crate::lexer::Lexer;

    fn parse_input(input: &str) -> Vec<Stmt> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();

        let debug_print_tokens = true;

        if debug_print_tokens {
            println!();
            for token in &tokens {
                println!("{:?}", token);
            }
            println!();
        }

        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn test_print_statement() {
        let input = "print 42;";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::Print(Expr::Literal(Literal::Number(value))) = &statements[0] {
            assert_eq!(*value, 42.0);
        } else {
            panic!("Expected print statement");
        }
    }

    #[test]
    fn test_if_statement() {
        let input = "if true do print 42; end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::If(cond, then_branch, _) = &statements[0] {
            if let Expr::Literal(Literal::Boolean(value)) = cond {
                assert_eq!(*value, true);
            } else {
                panic!("Expected boolean condition");
            }
            assert_eq!(then_branch.len(), 1);
            if let Stmt::Print(Expr::Literal(Literal::Number(value))) = &*then_branch[0] {
                assert_eq!(*value, 42.0);
            } else {
                panic!("Expected print statement in then branch");
            }
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_while_statement() {
        let input = "while true do print 42; end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::While(cond, body) = &statements[0] {
            if let Expr::Literal(Literal::Boolean(value)) = cond {
                assert_eq!(*value, true);
            } else {
                panic!("Expected boolean condition");
            }
            assert_eq!(body.len(), 1);
            if let Stmt::Print(Expr::Literal(Literal::Number(value))) = &*body[0] {
                assert_eq!(*value, 42.0);
            } else {
                panic!("Expected print statement in while body");
            }
        } else {
            panic!("Expected while statement");
        }
    }

    #[test]
    fn test_for_statement() {
        let input = "for i in 1..10 do print i; end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::For(var, start, end, body) = &statements[0] {
            assert_eq!(var, "i");
            if let Expr::Literal(Literal::Number(value)) = start {
                assert_eq!(*value, 1.0);
            } else {
                panic!("Expected number in range start");
            }
            if let Expr::Literal(Literal::Number(value)) = end {
                assert_eq!(*value, 10.0);
            } else {
                panic!("Expected number in range end");
            }
            assert_eq!(body.len(), 1);
            if let Stmt::Print(Expr::Identifier(name)) = &*body[0] {
                assert_eq!(name, "i");
            } else {
                panic!("Expected print statement in for loop body");
            }
        } else {
            panic!("Expected for statement");
        }
    }

    #[test]
    fn test_function_declaration() {
        let input = "def add(a: int, b: int) -> int do return a + b; end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);

        if let Stmt::FunctionDeclaration(name, params, return_type, body) = &statements[0] {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].0, "a");
            assert_eq!(params[0].1, "int");
            assert_eq!(params[1].0, "b");
            assert_eq!(params[1].1, "int");
            assert_eq!(return_type, "int");
            assert_eq!(body.len(), 1);

            if let Stmt::Return(Some(Expr::Binary(left, Operator::Add, right))) = &*body[0] {
                if let Expr::Identifier(ref left_name) = **left {
                    assert_eq!(left_name, "a");
                } else {
                    panic!("Expected identifier 'a' on left side of addition");
                }
                if let Expr::Identifier(ref right_name) = **right {
                    assert_eq!(right_name, "b");
                } else {
                    panic!("Expected identifier 'b' on right side of addition");
                }
            } else {
                panic!("Expected return statement with addition expression");
            }
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_class_declaration() {
        let input = "
            class Animal do
                name: string,
                def speak() -> string do
                    return \"...\";
                end
            end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::ClassDeclaration(name, fields, methods) = &statements[0] {
            assert_eq!(name, "Animal");
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "name");
            assert_eq!(fields[0].1, "string");
            assert_eq!(methods.len(), 1);
            if let Stmt::FunctionDeclaration(name, params, return_type, body) = &*methods[0] {
                assert_eq!(name, "speak");
                assert!(params.is_empty());
                assert_eq!(return_type, "string");
                assert_eq!(body.len(), 1);
                if let Stmt::Return(Some(Expr::Literal(Literal::String(value)))) = &*body[0] {
                    assert_eq!(value, "...");
                } else {
                    panic!("Expected return statement with string literal");
                }
            } else {
                panic!("Expected function declaration in class");
            }
        } else {
            panic!("Expected class declaration");
        }
    }

    #[test]
    fn test_schedule_statement() {
        let input = "schedule print 42; every: \"24h\"";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::Schedule(stmt, interval) = &statements[0] {
            assert_eq!(interval, "\"24h\"");
            if let Stmt::Print(Expr::Literal(Literal::Number(value))) = &**stmt {
                assert_eq!(*value, 42.0);
            } else {
                panic!("Expected print statement in schedule block");
            }
        } else {
            panic!("Expected schedule statement");
        }
    }

    #[test]
    fn test_macro_definition() {
        let input = "
            macro log_call(fn) do
                print \"Calling #{fn}\";
                fn();
            end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::MacroDefinition(name, params, body) = &statements[0] {
            assert_eq!(name, "log_call");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], "fn");
            assert_eq!(body.len(), 2);
            if let Stmt::Print(Expr::Literal(Literal::String(value))) = &*body[0] {
                assert_eq!(value, "\"Calling #{fn}\"");
            } else {
                panic!("Expected print statement in macro body");
            }
            if let Stmt::Expression(Expr::FunctionCall(fn_name, args)) = &*body[1] {
                assert_eq!(fn_name, "fn");
                assert!(args.is_empty());
            } else {
                panic!("Expected function call in macro body");
            }
        } else {
            panic!("Expected macro definition");
        }
    }

    #[test]
    fn test_foreign_function() {
        let input = "
            ffi \"c\" do
                def c_sqrt(x: float) -> float end
            end";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::ForeignFunction(lang, name, params) = &statements[0] {
            assert_eq!(lang, "\"c\"");
            assert_eq!(name, "c_sqrt");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].0, "x");
            assert_eq!(params[0].1, "float");
        } else {
            panic!("Expected foreign function interface declaration");
        }
    }

    #[test]
    fn test_binary_expression() {
        let input = "print 1 + 2 * 3;";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);
        if let Stmt::Print(expr) = &statements[0] {
            if let Expr::Binary(left, Operator::Add, right) = expr {
                if let Expr::Literal(Literal::Number(value)) = **left {
                    assert_eq!(value, 1.0);
                } else {
                    panic!("Expected number on left side of addition");
                }
                if let Expr::Binary(inner_left, Operator::Multiply, inner_right) = &**right {
                    if let Expr::Literal(Literal::Number(value)) = **inner_left {
                        assert_eq!(value, 2.0);
                    } else {
                        panic!("Expected number on left side of multiplication");
                    }
                    if let Expr::Literal(Literal::Number(value)) = **inner_right {
                        assert_eq!(value, 3.0);
                    } else {
                        panic!("Expected number on right side of multiplication");
                    }
                } else {
                    panic!("Expected multiplication on right side of addition");
                }
            } else {
                panic!("Expected addition expression");
            }
        } else {
            panic!("Expected print statement");
        }
    }

    #[test]
    fn test_property_access() {
        let input = "print user.name;";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);

        if let Stmt::Print(expr) = &statements[0] {
            if let Expr::PropertyAccess(ref object, ref property) = expr {
                if let Expr::Identifier(ref name) = **object {
                    assert_eq!(name, "user");
                } else {
                    panic!("Expected identifier 'user'");
                }
                assert_eq!(property, "name");
            } else {
                panic!("Expected property access expression");
            }
        } else {
            panic!("Expected print statement");
        }
    }

    #[test]
    fn test_index_access() {
        let input = "print array[2];";
        let statements = parse_input(input);
        assert_eq!(statements.len(), 1);

        if let Stmt::Print(expr) = &statements[0] {
            if let Expr::Index(ref object, ref index) = expr {
                if let Expr::Identifier(ref name) = **object {
                    assert_eq!(name, "array");
                } else {
                    panic!("Expected identifier 'array'");
                }
                if let Expr::Literal(Literal::Number(value)) = **index {
                    assert_eq!(value, 2.0);
                } else {
                    panic!("Expected number '2' as index");
                }
            } else {
                panic!("Expected index access expression");
            }
        } else {
            panic!("Expected print statement");
        }
    }
}
