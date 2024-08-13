use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub struct Lexer {
    source: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("if".to_string(), TokenType::Keyword);
        keywords.insert("else".to_string(), TokenType::Keyword);
        keywords.insert("while".to_string(), TokenType::Keyword);
        keywords.insert("def".to_string(), TokenType::Keyword);
        keywords.insert("return".to_string(), TokenType::Keyword);
        keywords.insert("print".to_string(), TokenType::Keyword);

        Lexer {
            source: source.chars().collect(),
            current: 0,
            line: 1,
            column: 1,
            keywords,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        while !self.is_at_end() {
            self.skip_whitespace(); // ignore whitespaces
            let start_column = self.column;

            let token = if let Some(c) = self.advance() {
                match c {
                    'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_keyword(start_column)?, // handle keywords/idents
                    '0'..='9' => self.number(start_column)?, // handle numbers
                    '"' => self.string_literal(start_column)?, // handle string literals
                    '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>' | '&' | '|' => self.operator(c, start_column), // handle operators
                    '(' | '{' | '[' => self.grouping(c, start_column)?, // handle grouping
                    ')' | '}' | ']' => return Err(format!("unmatched grouping character '{}' at line {}, column {}", c, self.line, start_column)), // handle unmatched closing groups
                    ',' | '.' | ';' | ':' => self.punctuation(c, start_column), // handle punctuation
                    '\n' => {
                        self.line += 1;
                        self.column = 0; // will increment to 1 at start of next iteration
                        Token::new(TokenType::Newline, "\n".to_string(), self.line, start_column, self.current_line())
                    },
                    _ => return Err(format!("unexpected character '{}' at line {}, column {}", c, self.line, self.column)), // handle unexpected chars
                }
            } else {
                break;
            };

            tokens.push(token); // push token to list
        }

        tokens.push(Token::new(TokenType::Eof, "".to_string(), self.line, self.column, self.current_line())); // end of file token
        Ok(tokens)
    }

    fn identifier_or_keyword(&mut self, start_column: usize) -> Result<Token, String> {
        let start = self.current - 1;
        while self.peek().map_or(false, |c| c.is_alphanumeric() || c == '_') {
            self.advance();
        }
        let lexeme: String = self.source[start..self.current].iter().collect();
        let token_type = self.keywords.get(&lexeme).cloned().unwrap_or(TokenType::Identifier);

        // check if it's a function or var declaration
        if token_type == TokenType::Identifier && self.peek() == Some('(') {
            return self.function_call_or_declaration(lexeme, start_column);
        }

        Ok(Token::new(token_type, lexeme, self.line, start_column, self.current_line()))
    }

    fn function_call_or_declaration(&mut self, name: String, start_column: usize) -> Result<Token, String> {
        // it's a function call/declaration if followed by '('
        self.advance(); // consume '('
        let mut children = vec![Token::new(TokenType::Identifier, name, self.line, start_column, self.current_line())];
        children.push(self.punctuation('(', start_column)); // add '('
    
        // gather all parameters
        while let Some(c) = self.peek() {
            if c == ')' {
                children.push(self.punctuation(')', self.column)); // add ')'
                self.advance();
                break;
            }
            children.push(self.tokenize()?); // recursive call to tokenize params
            if self.peek() == Some(',') {
                children.push(self.punctuation(',', self.column)); // add ','
                self.advance();
            }
        }
    
        if self.peek() == Some('{') {
            children.push(self.grouping('{', start_column)?); // group function body
            Ok(Token::with_children(TokenType::Group, "FunctionDeclaration".to_string(), self.line, start_column, children, self.current_line()))
        } else {
            Ok(Token::with_children(TokenType::Group, "FunctionCall".to_string(), self.line, start_column, children, self.current_line()))
        }
    }
    

    fn number(&mut self, start_column: usize) -> Result<Token, String> {
        let start = self.current - 1;
        while self.peek().map_or(false, |c| c.is_digit(10)) {
            self.advance();
        }

        if self.peek() == Some('.') && self.peek_next().map_or(false, |c| c.is_digit(10)) {
            self.advance(); // consume '.'
            while self.peek().map_or(false, |c| c.is_digit(10)) {
                self.advance();
            }
        }

        let lexeme: String = self.source[start..self.current].iter().collect();
        Ok(Token::new(TokenType::Number, lexeme, self.line, start_column, self.current_line()))
    }

    fn string_literal(&mut self, start_column: usize) -> Result<Token, String> {
        let start = self.current;
        while !self.is_at_end() && self.peek() != Some('"') {
            if self.peek() == Some('\n') {
                return Err(format!("unterminated string at line {}, column {}", self.line, self.column));
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(format!("unterminated string at line {}, column {}", self.line, self.column));
        }

        self.advance(); // consume the closing quote
        let lexeme: String = self.source[start..self.current - 1].iter().collect(); // exclude quotes
        Ok(Token::new(TokenType::StringLiteral, lexeme, self.line, start_column, self.current_line()))
    }

    fn operator(&mut self, first_char: char, start_column: usize) -> Token {
        let mut lexeme = first_char.to_string();

        if let Some(next_char) = self.peek() {
            match (first_char, next_char) {
                ('!', '=') | ('=', '=') | ('<', '=') | ('>', '=') | ('&', '&') | ('|', '|') => {
                    lexeme.push(self.advance().unwrap());
                }
                _ => {}
            }
        }

        Token::new(TokenType::Operator, lexeme, self.line, start_column, self.current_line())
    }

    fn punctuation(&mut self, char: char, start_column: usize) -> Token {
        Token::new(TokenType::Punctuation, char.to_string(), self.line, start_column, self.current_line())
    }

    fn grouping(&mut self, start_char: char, start_column: usize) -> Result<Token, String> {
        let matching_char = match start_char {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            _ => return Err(format!("invalid grouping character '{}'", start_char)),
        };
    
        let mut children = vec![Token::new(TokenType::Punctuation, start_char.to_string(), self.line, start_column, self.current_line())];
    
        while let Some(c) = self.peek() {
            if c == matching_char {
                children.push(self.punctuation(matching_char, self.column));
                self.advance();
                break;
            }
            children.push(self.tokenize()?); // recursively tokenize inner group
        }
    
        Ok(Token::with_children(TokenType::Group, start_char.to_string(), self.line, start_column, children, self.current_line()))
    }
    

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            let c = self.source[self.current];
            self.current += 1;
            self.column += 1;
            Some(c)
        }
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source[self.current])
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            None
        } else {
            Some(self.source[self.current + 1])
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn current_line(&self) -> String {
        self.source.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let source = "if else while def return print";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::Keyword, TokenType::Keyword, TokenType::Keyword,
            TokenType::Keyword, TokenType::Keyword, TokenType::Keyword,
            TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_identifiers() {
        let source = "variable1 variable_2 var123";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::Identifier, TokenType::Identifier, TokenType::Identifier,
            TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_numbers() {
        let source = "123 456.789 0.123 10";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::Number, TokenType::Number, TokenType::Number,
            TokenType::Number, TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_operators() {
        let source = "+ - * / == != < > <= >= && ||";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::Operator, TokenType::Operator, TokenType::Operator,
            TokenType::Operator, TokenType::Operator, TokenType::Operator,
            TokenType::Operator, TokenType::Operator, TokenType::Operator,
            TokenType::Operator, TokenType::Operator, TokenType::Operator,
            TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_strings() {
        let source = "\"hello\" \"world\" \"test123\"";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::StringLiteral, TokenType::StringLiteral, TokenType::StringLiteral,
            TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_punctuation() {
        let source = "( ) { } [ ] , . ; :";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        let expected = vec![
            TokenType::Punctuation, TokenType::Punctuation, TokenType::Punctuation,
            TokenType::Punctuation, TokenType::Punctuation, TokenType::Punctuation,
            TokenType::Punctuation, TokenType::Punctuation, TokenType::Punctuation,
            TokenType::Punctuation, TokenType::Eof
        ];

        assert_eq!(tokens.len(), expected.len());

        for (token, expected_type) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"hello";
        let mut lexer = Lexer::new(source);
        let result = lexer.tokenize();

        assert!(result.is_err());
    }
}
