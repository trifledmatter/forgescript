#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier,
    Keyword,
    Number,
    StringLiteral,
    Operator,
    Punctuation,
    Indent,
    Dedent,
    Newline,
    Group,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub children: Option<Vec<Token>>,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            children: None,
        }
    }

    pub fn with_children(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        children: Vec<Token>,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            children: Some(children),
        }
    }
}
