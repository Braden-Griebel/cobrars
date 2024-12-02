//! Module for parsing Gene Protein Reaction strings into AST values

use crate::io::gpr_parse::lexer::LexerError;
use crate::io::gpr_parse::parser::ParseError;
use crate::model::gene::{Gene, Gpr};
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

mod lexer;
mod parser;
mod token;

/// Parse a Gene Protein Reaction string into a GPR Tree
///
/// # Parameters
/// - `input`: &str representing the gene protein reaction rule
/// - `gene_map`: map of gene id strings to genes (wrapped in Rc<RefCell<>>)
///
/// # Returns
/// Parse result which is
/// - `Ok`: Includes a tuple of the root node of the GPR tree, and the updated `gene_map`.
/// - `Err`: Returns the GprParseError describing the issue with the GPR rule which
///     was being parsed.
///
/// # Examples
/// ```rust
/// use cobrars_core::io::gpr_parse::parse_gpr;
/// let gpr: &str = "Rv0001 and Rv0002";
/// let (gpr_tree, gene_map) = parse_gpr(gpr, None).unwrap();
/// ```
pub fn parse_gpr(
    input: &str,
    gene_map: Option<IndexMap<String, Rc<RefCell<Gene>>>>,
) -> Result<(Gpr, IndexMap<String, Rc<RefCell<Gene>>>), GprParseError> {
    // Start by creating a lexer
    let mut lexer = lexer::Lexer::new(input);
    // Convert the GPR string into tokens
    let tokens = match lexer.lex() {
        Ok(t) => t,
        Err(e) => {
            return Err(match e {
                LexerError::InvalidToken(s) => GprParseError::InvalidToken(s),
            })
        }
    };

    // Now parse those tokens into a GPR tree
    // first, if no gene_map is provided, create one
    let gene_map = gene_map.unwrap_or_else(|| IndexMap::new());
    // Create the parser
    let mut parser = parser::GPRParser::new(tokens, gene_map);
    // Parse the expression
    match parser.parse() {
        Ok(gpr) => Ok((gpr, parser.gene_map)),
        Err(e) => Err(match e {
            ParseError::InvalidBinaryOperator => GprParseError::InvalidBinaryOperator,
            ParseError::InvalidUnaryOperator => GprParseError::InvalidUnaryOperator,
            ParseError::MissingToken(s) => GprParseError::MissingToken(s),
            ParseError::ExpectedExpression => GprParseError::ExpectedExpression,
            ParseError::EarlyTermination => GprParseError::EarlyTermination,
        }),
    }
}

/// Enum representing possible lex and parse errors
#[derive(Debug)]
pub enum GprParseError {
    /// Invalid token found in
    InvalidToken(String),
    /// Token was expected to be a binary operator but was not
    InvalidBinaryOperator,
    /// Token was expected to be a unary operator but was not
    InvalidUnaryOperator,
    /// Missing expected token (e.g. a right parenthesis)
    MissingToken(String),
    /// No expression found when one was expected
    ExpectedExpression,
    /// Expression was not completed when parsing terminated
    EarlyTermination,
}

#[cfg(test)]
mod tests {
    use crate::io::gpr_parse::parse_gpr;
    use crate::model::gene::{Gene, GeneActivity, Gpr, GprOperation};
    use indexmap::IndexMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_parse_gpr() {
        let gpr = "Rv0001 and (Rv0002 or Rv0003)";
        let mut gene_map: IndexMap<String, Rc<RefCell<Gene>>> = IndexMap::new();
        gene_map.insert(
            "Rv0001".to_string(),
            Rc::new(RefCell::new(Gene::new(
                "Rv0001".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ))),
        );
        gene_map.insert(
            "Rv0002".to_string(),
            Rc::new(RefCell::new(Gene::new(
                "Rv0002".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ))),
        );
        gene_map.insert(
            "Rv0003".to_string(),
            Rc::new(RefCell::new(Gene::new(
                "Rv0003".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ))),
        );
        let (gpr_tree, gene_map) = parse_gpr(gpr, Some(gene_map)).unwrap();
        match gpr_tree {
            Gpr::Operation(op) => match op {
                GprOperation::And { left, right } => {
                    match *left {
                        Gpr::Gene(g) => {
                            if g.borrow().id != "Rv0001" {
                                panic!("Incorrect Parse")
                            }
                        }
                        _ => panic!("Incorrect Parse"),
                    }
                    match *right {
                        Gpr::Operation(GprOperation::Or { left, right }) => {
                            match *left {
                                Gpr::Gene(g) => {
                                    if g.borrow().id != "Rv0002" {
                                        panic!("Incorrect Parse")
                                    }
                                }
                                _ => panic!("Incorrect Parse"),
                            }
                            match *right {
                                Gpr::Gene(g) => {
                                    if g.borrow().id != "Rv0003" {
                                        panic!("Incorrect Parse")
                                    }
                                }
                                _ => panic!("Incorrect Parse"),
                            }
                        }
                        _ => panic!("Incorrect Parse"),
                    }
                }
                _ => {
                    panic!("Incorrect operation")
                }
            },
            Gpr::Gene(_) => {
                panic!("Incorrect gpr parse")
            }
        }
    }
}
