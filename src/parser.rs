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

    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.match_token(&[TokenType::Keyword]) {
            let keyword = self.previous().lexeme.as_str();
            match keyword {
                "def" => return self.function_declaration(),
                "type" => return self.type_declaration(),
                "class" => return self.class_declaration(),
                "module" => return self.module_declaration(),
                "macro" => return self.macro_definition(),
                "ffi" => return self.foreign_function(),
                "import" => return self.import_statement(),
                "mut" => return self.variable_declaration(true),
                _ => (),
            }
        }
        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_token(&[TokenType::Keyword]) {
            let keyword = self.previous().lexeme.as_str();
            match keyword {
                "print" => return self.print_statement(),
                "if" => return self.if_statement(),
                "while" => return self.while_statement(),
                "for" => return self.for_statement(),
                "return" => return self.return_statement(),
                "break" => return Ok(Stmt::Break),
                "continue" => return Ok(Stmt::Continue),
                "go" => return self.go_statement(),
                "schedule" => return self.schedule_statement(),
                _ => (),
            }
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")?;
        let then_branch = self.block()?;
        let mut else_branch = None;

        if self.match_token(&[TokenType::Keyword]) && self.previous().lexeme == "else" {
            self.consume(TokenType::Keyword, "Expect 'do' after 'else'.")?;
            else_branch = Some(self.block()?);
        }

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")?;
        let body = self.block()?;
        Ok(Stmt::While(condition, body))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        let variable = self.consume(TokenType::Identifier, "Expect variable name.")?;
        let variable_lexeme = variable.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'in' after variable.")?;

        let start = self.expression()?;

        self.consume(TokenType::Punctuation, "Expect '..' after start value.")?;

        let end = self.expression()?;

        self.consume(TokenType::Keyword, "Expect 'do' after range.")?;

        let body = self.block()?;

        Ok(Stmt::For(variable_lexeme, start, end, body))
    }

    fn return_statement(&mut self) -> Result<Stmt, String> {
        let expr = if self.check(TokenType::Punctuation) && self.peek().unwrap().lexeme == ";" {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Punctuation, "Expect ';' after return value.")?;
        Ok(Stmt::Return(expr))
    }

    fn go_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        Ok(Stmt::Go(Box::new(stmt)))
    }

    fn schedule_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        let interval = self.consume(
            TokenType::StringLiteral,
            "Expect string interval after schedule.",
        )?;
        Ok(Stmt::Schedule(Box::new(stmt), interval.lexeme.clone()))
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;

        if self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "=" {
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

    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "||" {
            let operator = Operator::Or;
            let right = self.and()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "&&" {
            let operator = Operator::And;
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::Operator])
            && (self.previous().lexeme == "==" || self.previous().lexeme == "!=")
        {
            let operator = match self.previous().lexeme.as_str() {
                "==" => Operator::Equal,
                "!=" => Operator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.match_token(&[TokenType::Operator])
            && (self.previous().lexeme == "<"
                || self.previous().lexeme == "<="
                || self.previous().lexeme == ">"
                || self.previous().lexeme == ">=")
        {
            let operator = match self.previous().lexeme.as_str() {
                "<" => Operator::Less,
                "<=" => Operator::LessEqual,
                ">" => Operator::Greater,
                ">=" => Operator::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_token(&[TokenType::Operator])
            && (self.previous().lexeme == "+" || self.previous().lexeme == "-")
        {
            let operator = match self.previous().lexeme.as_str() {
                "+" => Operator::Add,
                "-" => Operator::Subtract,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Operator])
            && (self.previous().lexeme == "*" || self.previous().lexeme == "/")
        {
            let operator = match self.previous().lexeme.as_str() {
                "*" => Operator::Multiply,
                "/" => Operator::Divide,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::Operator])
            && (self.previous().lexeme == "!" || self.previous().lexeme == "-")
        {
            let operator = match self.previous().lexeme.as_str() {
                "!" => Operator::Not,
                "-" => Operator::Subtract,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        while self.match_token(&[TokenType::Punctuation]) {
            let previous_token = self.previous().clone();
            match previous_token.lexeme.as_str() {
                "(" => expr = self.finish_call(expr)?,
                "." => expr = self.property_access(expr)?,
                "[" => expr = self.index_access(expr)?,
                _ => break,
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::Punctuation) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_token(&[TokenType::Punctuation]) || self.previous().lexeme != "," {
                    break;
                }
            }
        }
        self.consume(TokenType::Punctuation, "Expect ')' after arguments.")?;
        Ok(Expr::FunctionCall(callee.to_string(), arguments))
    }

    fn property_access(&mut self, object: Expr) -> Result<Expr, String> {
        let name = self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
        Ok(Expr::PropertyAccess(Box::new(object), name.lexeme.clone()))
    }

    fn index_access(&mut self, object: Expr) -> Result<Expr, String> {
        let index = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ']' after index.")?;
        Ok(Expr::Index(Box::new(object), Box::new(index)))
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::Boolean]) {
            return Ok(Expr::Literal(Literal::Boolean(
                self.previous().lexeme == "true",
            )));
        }

        if self.match_token(&[TokenType::Number]) {
            return Ok(Expr::Literal(Literal::Number(
                self.previous().lexeme.parse::<f64>().unwrap(),
            )));
        }

        if self.match_token(&[TokenType::StringLiteral]) {
            return Ok(Expr::Literal(Literal::String(
                self.previous().lexeme.clone(),
            )));
        }

        if self.match_token(&[TokenType::Null]) {
            return Ok(Expr::Literal(Literal::Null));
        }

        if self.match_token(&[TokenType::Identifier]) {
            return Ok(Expr::Identifier(self.previous().lexeme.clone()));
        }

        if self.match_token(&[TokenType::Punctuation]) && self.previous().lexeme == "(" {
            let expr = self.expression()?;
            self.consume(TokenType::Punctuation, "Expect ')' after expression.")?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        Err(format!(
            "Unexpected token {} at line {}, column {}",
            self.peek().unwrap().lexeme,
            self.peek().unwrap().line,
            self.peek().unwrap().column
        ))
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut statements = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            statements.push(Box::new(self.statement()?));
        }
        self.consume(TokenType::Keyword, "Expect 'end' after block.")?;
        Ok(statements)
    }

    fn function_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect function name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after function name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) {
            loop {
                let param_name = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param_name_lexeme = param_name.lexeme.clone();

                self.consume(TokenType::Punctuation, "Expect ':' after parameter name.")?;

                let param_type = self.consume(TokenType::Identifier, "Expect parameter type.")?;
                parameters.push((param_name_lexeme, param_type.lexeme.clone()));

                if !self.match_token(&[TokenType::Punctuation]) || self.previous().lexeme != "," {
                    break;
                }
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")?;
        self.consume(TokenType::Punctuation, "Expect '->' before return type.")?;

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
        }

        self.consume(TokenType::Keyword, "Expect 'end' after type declaration.")?;

        Ok(Stmt::TypeDeclaration(name_lexeme, fields))
    }

    fn class_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect class name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Keyword, "Expect 'do' after class name.")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            if self.match_token(&[TokenType::Identifier]) {
                let field_name = self.previous().lexeme.clone();

                self.consume(TokenType::Punctuation, "Expect ':' after field name.")?;

                let field_type = self.consume(TokenType::Identifier, "Expect field type.")?;
                fields.push((field_name, field_type.lexeme.clone()));
            } else if self.match_token(&[TokenType::Keyword]) && self.previous().lexeme == "def" {
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

    fn macro_definition(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect macro name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after macro name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) {
            loop {
                let param = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(param.lexeme.clone());
                if !self.match_token(&[TokenType::Punctuation]) || self.previous().lexeme != "," {
                    break;
                }
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")?;
        self.consume(TokenType::Keyword, "Expect 'do' before macro body.")?;

        let body = self.block()?;

        Ok(Stmt::MacroDefinition(name_lexeme, parameters, body))
    }

    fn foreign_function(&mut self) -> Result<Stmt, String> {
        let language = self.consume(
            TokenType::StringLiteral,
            "Expect language identifier for FFI.",
        )?;
        let language_lexeme = language.lexeme.clone();

        let name = self.consume(TokenType::Identifier, "Expect function name.")?;
        let name_lexeme = name.lexeme.clone();

        self.consume(TokenType::Punctuation, "Expect '(' after function name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) {
            loop {
                let param_name = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param_name_lexeme = param_name.lexeme.clone();

                self.consume(TokenType::Punctuation, "Expect ':' after parameter name.")?;

                let param_type = self.consume(TokenType::Identifier, "Expect parameter type.")?;
                let param_type_lexeme = param_type.lexeme.clone();

                parameters.push((param_name_lexeme, param_type_lexeme));

                if !self.match_token(&[TokenType::Punctuation]) || self.previous().lexeme != "," {
                    break;
                }
            }
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")?;
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

    fn import_statement(&mut self) -> Result<Stmt, String> {
        let module_name =
            self.consume(TokenType::StringLiteral, "Expect module name to import.")?;
        let module_name_lexeme = module_name.lexeme.clone();
        self.consume(TokenType::Punctuation, "Expect ';' after import statement.")?;
        Ok(Stmt::Import(module_name_lexeme))
    }

    fn variable_declaration(&mut self, mutable: bool) -> Result<Stmt, String> {
        let name_token = self.consume(TokenType::Identifier, "Expect variable name.")?;
        let name = name_token.lexeme.clone();

        let mut initializer = None;
        if self.match_token(&[TokenType::Operator]) {
            if self.previous().lexeme == "=" {
                initializer = Some(self.expression()?);
            }
        }

        self.consume(
            TokenType::Punctuation,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::VariableDeclaration(name, initializer, mutable))
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for &t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().unwrap().token_type == token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().unwrap().token_type == TokenType::Eof
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

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