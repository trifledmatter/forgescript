#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier,       // variable names, function names, etc.
    Keyword,          // reserved keywords like if, else, while, def, etc.
    Number,
    StringLiteral,
    Operator,         // +, -, *, /, etc.
    Punctuation,      // (, ), {, }, ;, etc.
    Indent,
    Dedent,
    Newline,
    Group,            // parentheses, brackets, etc.
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,   // string value of the token
    pub line: usize,      // ln where the token appears
    pub column: usize,    // cn where the token starts
    pub line_content: String, // entire line value for error reporting
    pub children: Option<Vec<Token>>,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize, line_content: String) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            line_content,
            children: None,
        }
    }

    pub fn with_children(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        line_content: String,
        children: Vec<Token>,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            line_content,
            children: Some(children),
        }
    }
    
    pub fn display_error(&self, message: &str) -> String {
        let mut result = format!("Error at line {}, column {}: {}\n", self.line, self.column, message);
        result.push_str(&self.line_content);
        result.push('\n');
        result.push_str(&" ".repeat(self.column - 1));
        result.push('^');
        result
    }
}
