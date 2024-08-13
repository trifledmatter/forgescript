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
            self.skip_whitespace();
            let start_column = self.column;

            let token = if let Some(c) = self.advance() {
                match c {
                    'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_keyword(start_column),
                    '0'..='9' => self.number(start_column),
                    '"' => self.string_literal(start_column)?,
                    '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>' | '&' | '|' => self.operator(c, start_column),
                    '(' | ')' | '{' | '}' | '[' | ']' | ',' | '.' | ';' | ':' => self.punctuation(c, start_column),
                    '\n' => {
                        self.line += 1;
                        self.column = 0; // increments to 1 at the start of the next iteration
                        Token::new(TokenType::Newline, "\n".to_string(), self.line, start_column, self.current_line())
                    },
                    _ => return Err(format!("Unexpected character '{}' at line {}, column {}", c, self.line, self.column)),
                }
            } else {
                break;
            };

            tokens.push(token);
        }

        tokens.push(Token::new(TokenType::Eof, "".to_string(), self.line, self.column, self.current_line()));
        Ok(tokens)
    }

    fn identifier_or_keyword(&mut self, start_column: usize) -> Token {
        let start = self.current - 1;
        while self.peek().map_or(false, |c| c.is_alphanumeric() || c == '_') {
            self.advance();
        }
        let lexeme: String = self.source[start..self.current].iter().collect();
        let token_type = self.keywords.get(&lexeme).cloned().unwrap_or(TokenType::Identifier);
        Token::new(token_type, lexeme, self.line, start_column, self.current_line())
    }

    fn number(&mut self, start_column: usize) -> Token {
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
        Token::new(TokenType::Number, lexeme, self.line, start_column, self.current_line())
    }

    fn string_literal(&mut self, start_column: usize) -> Result<Token, String> {
        let start = self.current;
        while !self.is_at_end() && self.peek() != Some('"') {
            if self.peek() == Some('\n') {
                return Err(format!("Unterminated string at line {}, column {}", self.line, self.column));
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}, column {}", self.line, self.column));
        }

        self.advance(); // consume closing quote
        let lexeme: String = self.source[start..self.current - 1].iter().collect(); // excl. quotes
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
        let mut line_start = self.current;
        while line_start > 0 && self.source[line_start - 1] != '\n' {
            line_start -= 1;
        }

        let line_end = self.source.iter().skip(line_start).position(|&c| c == '\n').unwrap_or(self.source.len() - line_start);

        self.source[line_start..line_start + line_end].iter().collect()
    }
}
