use crate::io::gpr_parse::token::Token;
use crate::core::gene::{Gpr, Gene, GeneActivity, GprOperatorType, GprError, GprOperation};

use std::rc::Rc;
use std::cell::RefCell;
use std::io::BufRead;
use indexmap::IndexMap;
use serde::Deserialize;
/*
GPR Grammar:
expression -> binary
binary -> unary (("AND" | "OR") unary )*;
unary -> "NOT" unary | primary  ;
primary -> GENE | "(" expression ")" ;

e.g. ( Gene1 AND Gene2) OR (Gene3 AND NOT Gene4)
 */

/// GPR Parser
pub struct GPRParser {
    /// Vector of tokens from the GPR string
    tokens: Vec<Token>,
    /// Current token being processed
    current: usize,
    /// Map containing the Genes
    gene_map: IndexMap<String, Rc<RefCell<Gene>>>,
}

impl GPRParser {
    /// Create a new GPRParser
    pub fn new(tokens: Vec<Token>, gene_map: IndexMap<String, Rc<RefCell<Gene>>>) -> GPRParser {
        GPRParser {
            tokens,
            current: 0,
            gene_map,
        }
    }

    // region Parsing Functions

    /// Parse the token vector into a GPR AST
    pub fn parse(&mut self) -> Result<Gpr, ParseError> {
        let gpr = self.binary()?;
        if !self.is_at_end() { // If entire expression has not been parsed, and error has occured
            return Err(ParseError::EarlyTermination)
        }
        Ok(gpr)
    }

    fn binary(&mut self) -> Result<Gpr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(vec![Token::And, Token::Or]) {
            let operator: GprOperatorType = match self.previous() {
                Token::Or => GprOperatorType::Or,
                Token::And => GprOperatorType::And,
                _ => return Err(ParseError::InvalidBinaryOperator)
            };
            let right = self.unary()?;
            let new_node = Gpr::new_binary_operation(expr, operator, right);
            expr = match new_node {
                Ok(gpr) => {gpr}
                Err(_) => {return Err(ParseError::InvalidBinaryOperator)}
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Gpr, ParseError> {
        if self.match_token(vec![Token::Not]) {
            let operator: GprOperatorType = match self.previous() {
                Token::Not => GprOperatorType::Not,
                _ => { return Err(ParseError::InvalidUnaryOperator) }
            };
            let right = self.unary()?;
            return Ok(match Gpr::new_unary_operation(operator, right){
                Ok(gpr) => {gpr}
                Err(err) => {
                    return match err {
                        GprError::InvalidUnaryOp => { Err(ParseError::InvalidUnaryOperator) }
                        _ => { Err(ParseError::InvalidUnaryOperator) } // This should be impossible
                    }
                }
            });
        }
        Ok(self.primary()?)
    }

    fn primary(&mut self) -> Result<Gpr, ParseError> {
        if let Some(identifier) = self.match_identifier() {
            return Ok(Gpr::new_gene_node(self.get_or_insert_gene(identifier)));
        }

        if self.match_token(vec![Token::LeftParen]){
            let expr = self.binary()?;
            self.consume(Token::RightParen, "Expect ')' after expression.")?;
            return Ok(expr);
        }

        Err(ParseError::ExpectedExpression)
    }

    // endregion Parsing Functions

    // region parsing helper functions

    /// Check whether the token at the current position matches one of the provided `tokens`,
    /// if it does advance [`self.current`] and return true, otherwise return false
    fn match_token(&mut self, tokens: Vec<Token>)-> bool {
        for t in tokens {
            if self.check(t){
                self.advance();
                return true;
            }
        }
        false
    }

    /// Similar to [`match_token`], but for matching an identifier token. If the current
    /// token is an identifier return `Some(GeneId)`, where GeneId is the gene's string identifier,
    /// otherwise return None
    fn match_identifier(&mut self) -> Option<String> {
        if self.is_at_end() { return None}
        if let Token::Identifier(id) = self.peek(){
            self.advance();
            return Some(id);
        }
        None
    }

    /// Check whether the current token matches the provided `token`
    fn check(&mut self, token: Token) -> bool {
        if self.is_at_end() { return false;}
        self.peek() == token
    }

    /// Advance `self.current` one position unless at end of GPR Vec, then return the previous
    /// token.
    fn advance(&mut self) -> Token {
        if !self.is_at_end(){self.current+=1;}
        self.previous()
    }

    /// Check whether the parser is at the end of the source Vec
    fn is_at_end(&self) -> bool {
        self.peek() == Token::Eof
    }

    /// Get a copy of the current token
    fn peek(&self)-> Token {
        self.tokens[self.current].clone()
    }

    /// Get a copy of the previous token
    fn previous(&self)-> Token{
        self.tokens[self.current-1].clone()
    }

    /// Check whether the current token matches an input token, if it matches advance to the
    /// next token, and if it doesn't return an error. Used mainly for matching parenthesis in
    /// source GPR vec.
    fn consume(&mut self, token: Token, msg: &str) -> Result<Token, ParseError> {
        if self.check(token) {return Ok(self.advance())}

        Err(ParseError::MissingToken(msg.to_string()))

    }

    // endregion parsing helper functions

    // region Gene Map Functions

    /// Get a reference to a gene in [`gene_map`] if it exists, or insert a new gene with the
    /// provided id
    fn get_or_insert_gene(&mut self, gene_id: String) -> Rc<RefCell<Gene>> {
        self.gene_map.entry(gene_id.clone()).or_insert(
            Rc::new(RefCell::new(Gene::new(gene_id, None, GeneActivity::Active)))
        ).clone()
    }

    // endregion Gene Map Functions

}

/// Enum representing possible parse errors
#[derive(Debug)]
pub enum ParseError {
    /// Token was expected to be a binary operator but was not
    InvalidBinaryOperator,
    /// Token was expected to be a unary operator but was not
    InvalidUnaryOperator,
    /// Missing expected token (e.g. a right parenthesis)
    MissingToken(String),
    /// No expression found when one was expected
    ExpectedExpression,
    /// Expression was not completed when parsing terminated
    EarlyTermination
}

#[cfg(test)]
mod tests {
    use crate::io::gpr_parse::lexer::Lexer;
    use super::*;

    #[test]
    fn test_single_gene_parse(){
        let mut lexer = Lexer::new("Rv1304");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(_) => {
                panic!("Incorrect Parse Result (Should have been single gene)")
            }
            Gpr::Gene(gene) => {
                if (*gene).borrow().id != "Rv1304".to_string(){
                    panic!("Wrong Gene");
                }
            }
        }
    }

    #[test]
    fn test_and_parse(){
        let mut lexer = Lexer::new("Rv1304 and Rv0023");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(op) => {
                match op {

                    GprOperation::And { left, right } => {
                        match *left {
                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv1304".to_string(){
                                    panic!("Incorrect Left Gene");
                                }
                            }
                        }
                        match *right {
                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv0023".to_string(){
                                    panic!("Incorrect Right Gene");
                                }
                            }
                        }
                    }
                    GprOperation::Not { .. } => {
                        panic!("Should have been an AND operation")
                    }
                    GprOperation::Or { .. } => {
                        panic!("Should have been an AND operation")
                    }
                }
            }
            Gpr::Gene(_) => {panic!("Incorrect Parse Result (Should have been an AND operation)")}
        }
    }

    #[test]
    fn test_or_parse(){
        let mut lexer = Lexer::new("Rv1304 or Rv0023");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(op) => {
                match op {
                    GprOperation::Or { left, right } => {
                        match *left {
                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv1304".to_string(){
                                    panic!("Incorrect Left Gene");
                                }
                            }
                        }
                        match *right {
                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv0023".to_string(){
                                    panic!("Incorrect Right Gene");
                                }
                            }
                        }
                    }
                    GprOperation::Not { .. } => {
                        panic!("Should have been an OR operation")
                    }
                    GprOperation::And { .. } => {
                        panic!("Should have been an OR operation")
                    }
                }
            }
            Gpr::Gene(_) => {panic!("Incorrect Parse Result (Should have been an AND operation)")}
        }
    }

    #[test]
    fn test_not_parse(){
        let mut lexer = Lexer::new("not Rv0023");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(op) => {
                match op {
                    GprOperation::Not { val } => {
                        match *val {
                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv0023".to_string(){
                                    panic!("Incorrect Left Gene");
                                }
                            }
                        }
                    }
                    _ => {panic!("Incorrect Operation Parsed")}
                }
            }
            Gpr::Gene(_) => {panic!("Incorrect Parse Result (Should have been an OR operation)")}
        }
    }

    #[test]
    fn test_grouping_parse(){
        let mut lexer = Lexer::new("(Rv3141 or Rv0023) and Rv018");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(op) => {
                match op {
                    GprOperation::And { left, right } => {
                        match *left {
                            Gpr::Operation(op) => {
                                match op {
                                    GprOperation::Or { left, right } => {
                                        match *left {
                                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                                            Gpr::Gene(gene_ref) => {
                                                if gene_ref.borrow().id != "Rv3141".to_string(){
                                                    panic!("Incorrect Left Gene");
                                                }
                                            }
                                        }
                                        match *right {
                                            Gpr::Operation(_) => {panic!("Should have been a gene")}
                                            Gpr::Gene(gene_ref) => {
                                                if gene_ref.borrow().id != "Rv0023".to_string(){
                                                    panic!("Incorrect Right Gene");
                                                }
                                            }
                                        }
                                    }
                                    _ => {panic!("Incorrect Operation Parsed")}
                                }
                            }
                            _ => {panic!("Should have parsed an OR operation")}
                        }
                        match *right {
                            Gpr::Operation(_) => {panic!("Should Have Been a Gene Parsed")}
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv0018".to_string(){}
                            }
                        }
                    }
                    _ => {panic!("Incorrect Operation Parsed")}
                }
            }
            _ => {panic!("Incorrect Parse (should have been an AND operation)")}
        }
    }

    #[test]
    fn test_repeated_binary_parse(){
        let mut lexer = Lexer::new("Rv0001 and Rv0002 and Rv0003");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        let gpr_res = parser.parse().unwrap();
        match gpr_res {
            Gpr::Operation(op) => {
                match op {
                    GprOperation::And { left, right } => {
                        match *right {
                            Gpr::Gene(gene_ref) => {
                                if gene_ref.borrow().id != "Rv0003".to_string(){
                                    panic!("Incorrect Left Gene");
                                }
                            }
                            _=> panic!("Incorrect parse")
                        }
                        match *left {
                            Gpr::Operation(op) => {
                                match op {
                                    GprOperation::And { left, right } => {
                                        match *left {
                                            Gpr::Gene(gene_ref) => {
                                                if gene_ref.borrow().id != "Rv0001".to_string(){
                                                    panic!("Incorrect Left Gene");
                                                }
                                            }
                                            _=> panic!("Incorrect parse")
                                        }
                                        match *right {
                                            Gpr::Gene(gene_ref) => {
                                                if gene_ref.borrow().id != "Rv0002".to_string(){
                                                    panic!("Incorrect Right Gene");
                                                }
                                            }
                                            _=> panic!("Incorrect parse")
                                        }
                                    }
                                    _=> panic!("Incorrect parse")
                                }
                            }
                            _=> panic!("Incorrect parse")
                        }
                    }
                    _=> panic!("Incorrect parse")
                }
            }
            _=> panic!("Incorrect parse")
        }
    }

    #[test]
    fn invalid_parse(){
        let mut lexer = Lexer::new("Rv0001 not Rv0023");
        let token_vec: Vec<Token> = lexer.lex().unwrap();
        let mut parser = GPRParser::new(token_vec, IndexMap::new());
        match parser.parse(){
            Ok(gpr) => {println!("{}", gpr.to_string_id())}
            Err(err) => {
                match err {
                    ParseError::EarlyTermination => {}
                    _=>{panic!("Should have errored")}
                }
            }
        }
    }
}