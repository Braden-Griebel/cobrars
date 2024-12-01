//! Lex a GPR string into a series of tokens for later parsing

use std::collections::VecDeque;
use std::borrow::Borrow;


use crate::io::gpr_parse::token::Token;

/// Struct for lexing a GPR string into a token stream
pub struct Lexer {
    /// GPR source as a character vector
    source: Vec<char>,
    /// Output token stream
    tokens: VecDeque<Token>,
    /// Start of the current token
    start: usize,
    /// Current position in source
    current: usize,
}

impl Lexer {
    /// Create a new lexer
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.chars().collect(),
            tokens: VecDeque::new(),
            start: 0,
            current: 0,
        }
    }

    /// Lex the source code, returning a vector of tokens
    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let token_deque = self.scan_tokens()?;
        Ok(Vec::from(token_deque.clone()))
    }

    /// Scan the source GPR and return a token stream
    pub fn scan_tokens(&mut self)->Result<&VecDeque<Token>, LexerError>{
        while !self.is_at_end(){
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push_back(Token::Eof);
        Ok(&self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), LexerError>{
        let c:char = self.advance();
        match c{
            // Single Character Tokens
            '(' => self.add_token(Token::LeftParen),
            ')' => self.add_token(Token::RightParen),
            // Identifiers and Operators
            'a'..='z'|'A'..='Z'|'_' => {self.read_identifier()}
            // Whitespace
            ' '|'\r'|'\n'|'\t' => {}
            // Error Case (shouldn't happen)
            _ => {return Err(LexerError::InvalidToken)},
        };
        Ok(())
    }

    fn advance(&mut self) -> char{
        let char_at_current = self.source[self.current];
        self.current += 1;
        char_at_current
    }

    fn read_identifier(&mut self){
        while Lexer::is_alphanumeric(self.peek()){
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        match text.borrow() {
            "and"|"And"|"AND" => {self.add_token(Token::And)},
            "or"|"Or"|"OR" => {self.add_token(Token::Or)},
            "not"|"Not"|"NOT" => {self.add_token(Token::Not)},
            gene => {self.add_token(Token::Identifier(gene.to_string()))},
        }
    }

    fn is_digit(c:char)->bool{
        match c {
            '0'..='9'=>true,
            _=>false
        }
    }

    fn is_alpha(c: char)->bool{
        match c {
            'a'..='z'|'A'..='Z'|'_'=>true,
            _=>false,
        }
    }

    fn is_alphanumeric(c: char)->bool{
        Lexer::is_alpha(c) || Lexer::is_digit(c)
    }

    fn peek(&self)->char{
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current]
    }

    fn add_token(&mut self, token: Token){
        self.tokens.push_back(token);
    }

     fn is_at_end(&self)->bool {
        self.current >= self.source.len()
    }
}

#[derive(Debug, Clone)]
pub enum LexerError{
    InvalidToken,
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;
    use crate::io::gpr_parse::lexer::Lexer;
    use crate::io::gpr_parse::token::Token;

    #[test]
    fn test_single_gene(){
        let mut lexer = Lexer::new("Rv0023");
        let mut tokens = match lexer.scan_tokens(){
            Ok(t) => t,
            Err(_)=> panic!("Failed to parse during test")
        };
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens.get(0).unwrap(), &Token::Identifier(String::from("Rv0023")));
    }

    #[test]
    fn test_grouping(){
        let mut lexer = Lexer::new("(Rv0023 or Rv0123)");
        let mut tokens = match lexer.scan_tokens(){
            Ok(t) => t,
            Err(_)=> panic!("Failed to parse during test")
        };
        assert_eq!(tokens.len(), 6);
        let mut expected_tokens = VecDeque::new();
        expected_tokens.push_back(Token::LeftParen);
        expected_tokens.push_back(Token::Identifier(String::from("Rv0023")));
        expected_tokens.push_back(Token::Or);
        expected_tokens.push_back(Token::Identifier(String::from("Rv0123")));
        expected_tokens.push_back(Token::RightParen);
        expected_tokens.push_back(Token::Eof);
        assert_eq!(tokens, &expected_tokens);
    }
}