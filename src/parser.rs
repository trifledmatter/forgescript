use crate::ast::{Expr, Literal, Operator, Stmt};
use crate::token::{Token, TokenType};
use std::boxed::Box;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Box<Stmt>>, Vec<String>> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(Box::new(stmt)),
                Err(err) => self.errors.push(err),
            }
        }

        if self.errors.is_empty() {
            Ok(statements)
        } else {
            Err(self.errors.clone())
        }
    }

    // -- Declaration Parsing -- //

    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.match_token(&[TokenType::Keyword]) {
            match self.previous().lexeme.as_str() {
                "type" => self.type_declaration(),
                "class" => self.class_declaration(),
                "def" => self.function_declaration(),
                "mut" => self.variable_declaration(true),
                "import" => self.import_statement(),
                "module" => self.module_declaration(),
                "go" => self.go_statement(),
                "schedule" => self.schedule_statement(),
                _ => self.statement(),
            }
        } else {
            self.statement()
        }
    }

    fn type_declaration(&mut self) -> Result<Stmt, String> {
        let name = self
            .consume(TokenType::Identifier, "Expect type name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Keyword, "Expect 'do' after type name.")?;

        let mut fields = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            let field_name = self
                .consume(TokenType::Identifier, "Expect field name.")?
                .lexeme
                .clone();
            self.consume(TokenType::Punctuation, "Expect ':' after field name.")?;
            let field_type = self
                .consume(TokenType::Identifier, "Expect field type.")?
                .lexeme
                .clone();
            fields.push((field_name, field_type));
        }

        self.consume(TokenType::Keyword, "Expect 'end' after type declaration.")?;
        Ok(Stmt::TypeDeclaration(name, fields))
    }

    fn class_declaration(&mut self) -> Result<Stmt, String> {
        let name = self
            .consume(TokenType::Identifier, "Expect class name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Keyword, "Expect 'do' after class name.")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            if self.match_token(&[TokenType::Identifier]) {
                fields.push(self.field_declaration()?);
            } else {
                methods.push(Box::new(self.function_declaration()?));
            }
        }

        self.consume(TokenType::Keyword, "Expect 'end' after class declaration.")?;
        Ok(Stmt::ClassDeclaration(name, fields, methods))
    }

    fn function_declaration(&mut self) -> Result<Stmt, String> {
        let name = self
            .consume(TokenType::Identifier, "Expect function name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Punctuation, "Expect '(' after function name.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::Punctuation) {
            while {
                let param_name = self
                    .consume(TokenType::Identifier, "Expect parameter name.")?
                    .lexeme
                    .clone();
                self.consume(TokenType::Punctuation, "Expect ':' after parameter name.")?;
                let param_type = self
                    .consume(TokenType::Identifier, "Expect parameter type.")?
                    .lexeme
                    .clone();
                parameters.push((param_name, param_type));
                self.match_token(&[TokenType::Punctuation])
            } {}
        }

        self.consume(TokenType::Punctuation, "Expect ')' after parameters.")?;
        self.consume(TokenType::Operator, "Expect '->' after parameters.")?;
        let return_type = self
            .consume(TokenType::Identifier, "Expect return type.")?
            .lexeme
            .clone();

        self.consume(TokenType::Keyword, "Expect 'do' to start function body.")?;
        let body = self.block()?;
        self.consume(TokenType::Keyword, "Expect 'end' after function body.")?;

        Ok(Stmt::FunctionDeclaration(name, parameters, return_type, body))
    }

    fn variable_declaration(&mut self, is_mutable: bool) -> Result<Stmt, String> {
        let name = self
            .consume(TokenType::Identifier, "Expect variable name.")?
            .lexeme
            .clone();
        let initializer = if self.match_token(&[TokenType::Operator]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            TokenType::Punctuation,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::VariableDeclaration(name, initializer, is_mutable))
    }

    fn import_statement(&mut self) -> Result<Stmt, String> {
        let module_name = self
            .consume(TokenType::StringLiteral, "Expect module name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Punctuation, "Expect ';' after import statement.")?;
        Ok(Stmt::Import(module_name))
    }

    fn module_declaration(&mut self) -> Result<Stmt, String> {
        let name = self
            .consume(TokenType::Identifier, "Expect module name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Keyword, "Expect 'do' after module name.")?;
        let body = self.block()?;
        self.consume(TokenType::Keyword, "Expect 'end' after module body.")?;
        Ok(Stmt::Module(name, body))
    }

    fn go_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        Ok(Stmt::Go(Box::new(stmt)))
    }

    fn schedule_statement(&mut self) -> Result<Stmt, String> {
        let stmt = self.statement()?;
        self.consume(TokenType::Keyword, "Expect 'every' in schedule statement.")?;
        let interval = self
            .consume(TokenType::StringLiteral, "Expect interval string.")?
            .lexeme
            .clone();
        Ok(Stmt::Schedule(Box::new(stmt), interval))
    }

    fn field_declaration(&mut self) -> Result<(String, String), String> {
        let field_name = self
            .consume(TokenType::Identifier, "Expect field name.")?
            .lexeme
            .clone();
        self.consume(TokenType::Punctuation, "Expect ':' after field name.")?;
        let field_type = self
            .consume(TokenType::Identifier, "Expect field type.")?
            .lexeme
            .clone();
        Ok((field_name, field_type))
    }

    // -- Statement Parsing -- //

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_token(&[TokenType::Keyword]) {
            match self.previous().lexeme.as_str() {
                "if" => self.if_statement(),
                "while" => self.while_statement(),
                "for" => self.for_statement(),
                "foreach" => self.foreach_statement(),
                "return" => self.return_statement(),
                "break" => Ok(Stmt::Break),
                "continue" => Ok(Stmt::Continue),
                "print" => self.print_statement(),
                _ => self.expression_statement(),
            }
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")?;
        let then_branch = self.block()?;
        let else_branch =
            if self.match_token(&[TokenType::Keyword]) && self.previous().lexeme == "else" {
                Some(self.block()?)
            } else {
                None
            };
        self.consume(TokenType::Keyword, "Expect 'end' after if statement.")?;
        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        let condition = self.expression()?;
        self.consume(TokenType::Keyword, "Expect 'do' after condition.")?;
        let body = self.block()?;
        self.consume(TokenType::Keyword, "Expect 'end' after while statement.")?;
        Ok(Stmt::While(condition, body))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::Punctuation, "Expect '(' after 'for'.")?;
        let initializer = self.variable_declaration(true)?;
        let condition = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ';' after loop condition.")?;
        let increment = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ')' after for clauses.")?;
        self.consume(TokenType::Keyword, "Expect 'do' to start loop body.")?;

        let body = self.block()?;
        self.consume(TokenType::Keyword, "Expect 'end' after for loop.")?;
        Ok(Stmt::For(
            initializer.to_string(),
            condition,
            increment,
            body,
        ))
    }

    fn foreach_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::Punctuation, "Expect '(' after 'foreach'.")?;
        let variable = self
            .consume(TokenType::Identifier, "Expect identifier.")?
            .lexeme
            .clone();
        self.consume(TokenType::Keyword, "Expect 'in' after identifier.")?;
        let collection = self.expression()?;
        self.consume(
            TokenType::Punctuation,
            "Expect ')' after collection expression.",
        )?;
        self.consume(TokenType::Keyword, "Expect 'do' to start foreach body.")?;

        let body = self.block()?;
        self.consume(TokenType::Keyword, "Expect 'end' after foreach loop.")?;
        Ok(Stmt::ForEach(variable, collection, body))
    }

    fn return_statement(&mut self) -> Result<Stmt, String> {
        let value = if !self.check(TokenType::Punctuation) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Punctuation, "Expect ';' after return value.")?;
        Ok(Stmt::Return(value))
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(TokenType::Punctuation, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn block(&mut self) -> Result<Vec<Box<Stmt>>, String> {
        let mut statements = Vec::new();
        while !self.check(TokenType::Keyword) && !self.is_at_end() {
            statements.push(Box::new(self.declaration()?));
        }
        Ok(statements)
    }

    // -- Expression Parsing -- //

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.ternary()?;

        if self.match_token(&[TokenType::Operator]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Identifier(name) = expr {
                return Ok(Expr::Assignment(
                    Box::new(Expr::Identifier(name)),
                    Box::new(value),
                ));
            } else {
                return Err(format!("Invalid assignment target at line {}", equals.line));
            }
        }

        Ok(expr)
    }

    fn ternary(&mut self) -> Result<Expr, String> {
        let mut expr = self.or()?;

        if self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "?" {
            let then_branch = self.expression()?;
            self.consume(
                TokenType::Operator,
                "Expect ':' after then branch of ternary expression.",
            )?;
            let else_branch = self.ternary()?;
            expr = Expr::Ternary(Box::new(expr), Box::new(then_branch), Box::new(else_branch));
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "||" {
            let right = self.and()?;
            expr = Expr::Binary(Box::new(expr), Operator::Or, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.match_token(&[TokenType::Operator]) && self.previous().lexeme == "&&" {
            let right = self.equality()?;
            expr = Expr::Binary(Box::new(expr), Operator::And, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_token(&[TokenType::Operator])
            && ["==", "!="].contains(&self.previous().lexeme.as_str())
        {
            let operator = self.previous().lexeme.clone();
            let right = self.comparison()?;
            let op = if operator == "==" {
                Operator::Equal
            } else {
                Operator::NotEqual
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.match_token(&[TokenType::Operator])
            && ["<", "<=", ">", ">="].contains(&self.previous().lexeme.as_str())
        {
            let operator = self.previous().lexeme.clone();
            let right = self.term()?;
            let op = match operator.as_str() {
                "<" => Operator::Less,
                "<=" => Operator::LessEqual,
                ">" => Operator::Greater,
                ">=" => Operator::GreaterEqual,
                _ => return Err("Invalid comparison operator".to_string()),
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_token(&[TokenType::Operator])
            && ["+", "-"].contains(&self.previous().lexeme.as_str())
        {
            let operator = self.previous().lexeme.clone();
            let right = self.factor()?;
            let op = if operator == "+" {
                Operator::Add
            } else {
                Operator::Subtract
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Operator])
            && ["*", "/", "%"].contains(&self.previous().lexeme.as_str())
        {
            let operator = self.previous().lexeme.clone();
            let right = self.unary()?;
            let op = if operator == "*" {
                Operator::Multiply
            } else if operator == "/" {
                Operator::Divide
            } else {
                Operator::Modulo
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::Operator])
            && ["!", "-", "~"].contains(&self.previous().lexeme.as_str())
        {
            let operator = self.previous().lexeme.clone();
            let right = self.unary()?;
            let op = match operator.as_str() {
                "!" => Operator::Not,
                "-" => Operator::Subtract,
                "~" => Operator::BitwiseNot,
                _ => return Err("Invalid unary operator".to_string()),
            };
            return Ok(Expr::Unary(op, Box::new(right)));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenType::Punctuation]) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(&[TokenType::Punctuation]) {
                let property = self
                    .consume(TokenType::Identifier, "Expect property name after '.'.")?
                    .lexeme
                    .clone();
                expr = Expr::PropertyAccess(Box::new(expr), property);
            } else if self.match_token(&[TokenType::Punctuation]) {
                let index = self.expression()?;
                self.consume(TokenType::Punctuation, "Expect ']' after index expression.")?;
                expr = Expr::Index(Box::new(expr), Box::new(index));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::Punctuation) {
            while {
                arguments.push(self.expression()?);
                self.match_token(&[TokenType::Punctuation])
            } {}
        }

        self.consume(TokenType::Punctuation, "Expect ')' after arguments.")?;
        Ok(Expr::FunctionCall(callee.to_string(), arguments))
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(&[TokenType::Number]) {
            return Ok(Expr::Literal(Literal::Number(
                self.previous().lexeme.parse().unwrap(),
            )));
        }
        if self.match_token(&[TokenType::StringLiteral]) {
            return Ok(Expr::Literal(Literal::String(
                self.previous().lexeme.clone(),
            )));
        }
        if self.match_token(&[TokenType::Boolean]) {
            return Ok(Expr::Literal(Literal::Boolean(
                self.previous().lexeme == "true",
            )));
        }
        if self.match_token(&[TokenType::Null]) {
            return Ok(Expr::Literal(Literal::Null));
        }
        if self.match_token(&[TokenType::Identifier]) {
            return Ok(Expr::Identifier(self.previous().lexeme.clone()));
        }
        if self.match_token(&[TokenType::Punctuation]) {
            let expr = self.expression()?;
            self.consume(TokenType::Punctuation, "Expect ')' after expression.")?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        Err(format!(
            "Unexpected token '{}' at line {}, column {}",
            self.peek().lexeme,
            self.peek().line,
            self.peek().column
        ))
    }

    // -- Utility Functions -- //

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for &t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(self.peek().display_error(message))
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_source(source: &str) -> Result<Vec<Box<Stmt>>, Vec<String>> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_basic_expression() {
        let source = "1 + 2 * 3;";
        let result = parse_source(source);
        assert!(result.is_ok(), "Parser failed on valid expression.");
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        // Add further assertions to check the structure of the parsed expression
    }

    #[test]
    fn test_variable_declaration() {
        let source = "mut x = 42;";
        let result = parse_source(source);
        assert!(
            result.is_ok(),
            "Parser failed on valid variable declaration."
        );
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        if let Stmt::VariableDeclaration(name, Some(Expr::Literal(Literal::Number(value))), is_mut) =
            &*stmts[0]
        {
            assert_eq!(name, "x");
            assert_eq!(*value, 42.0);
            assert_eq!(*is_mut, true);
        } else {
            panic!("Unexpected AST structure for variable declaration.");
        }
    }

    #[test]
    fn test_if_statement() {
        let source = "if x > 10 do print(x); end";
        let result = parse_source(source);
        assert!(result.is_ok(), "Parser failed on valid if statement.");
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);

        if let Stmt::If(condition, then_branch, None) = &*stmts[0] {
            if let Expr::Binary(left, Operator::Greater, right) = condition {
                if let Expr::Identifier(name) = &**left {
                    assert_eq!(name, "x");
                } else {
                    panic!("Unexpected AST structure for if condition.");
                }

                if let Expr::Literal(Literal::Number(value)) = &**right {
                    assert_eq!(*value, 10.0);
                } else {
                    panic!("Unexpected AST structure for if condition.");
                }
            } else {
                panic!("Unexpected AST structure for if condition.");
            }
            assert_eq!(then_branch.len(), 1);
        } else {
            panic!("Unexpected AST structure for if statement.");
        }
    }

    #[test]
    fn test_function_declaration() {
        let source = "def add(a: int, b: int) -> int do return a + b; end";
        let result = parse_source(source);
        assert!(
            result.is_ok(),
            "Parser failed on valid function declaration."
        );
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        if let Stmt::FunctionDeclaration(name, params, return_type, body) = &*stmts[0] {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], ("a".to_string(), "int".to_string()));
            assert_eq!(params[1], ("b".to_string(), "int".to_string()));
            assert_eq!(return_type, "int");
            assert_eq!(body.len(), 1);
        } else {
            panic!("Unexpected AST structure for function declaration.");
        }
    }

    #[test]
    fn test_type_declaration() {
        let source = "type Point do x: int y: int end";
        let result = parse_source(source);
        assert!(result.is_ok(), "Parser failed on valid type declaration.");
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        if let Stmt::TypeDeclaration(name, fields) = &*stmts[0] {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[0].1, "int");
            assert_eq!(fields[1].0, "y");
            assert_eq!(fields[1].1, "int");
        } else {
            panic!("Unexpected AST structure for type declaration.");
        }
    }

    #[test]
    fn test_class_declaration() {
        let source = "class Rectangle do def area() -> int do return width * height; end end";
        let result = parse_source(source);
        assert!(result.is_ok(), "Parser failed on valid class declaration.");
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1);
        if let Stmt::ClassDeclaration(name, _, methods) = &*stmts[0] {
            assert_eq!(name, "Rectangle");
            assert_eq!(methods.len(), 1);
        } else {
            panic!("Unexpected AST structure for class declaration.");
        }
    }

    #[test]
    fn test_error_handling() {
        let source = "mut x = ;";
        let result = parse_source(source);
        assert!(
            result.is_err(),
            "Parser did not catch error in variable declaration."
        );
    }
}
