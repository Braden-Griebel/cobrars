//! Module for parsing Gene Protein Reaction strings into AST values

use crate::io::gpr_parse::lexer::LexerError;
use crate::io::gpr_parse::parser::ParseError;
use crate::model::gene::{Gene, Gpr};
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

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
    let tokens = lexer.lex()?;

    // Now parse those tokens into a GPR tree
    // first, if no gene_map is provided, create one
    let gene_map = gene_map.unwrap_or_default();
    // Create the parser
    let mut parser = parser::GPRParser::new(tokens, gene_map);
    // Parse the expression
    let gpr = parser.parse()?;
    Ok((gpr, parser.gene_map))
}

/// Enum representing possible lex and parse errors
#[derive(Debug, Error)]
pub enum GprParseError {
    /// Lexing Error
    #[error("Error occurred during lexing (conversion of GPR string to tokens)")]
    LexingError(#[from] LexerError),
    /// Parsing Error
    #[error("Error occurred during parsing (conversion of tokens to GPR tree)")]
    ParsingError(#[from] ParseError),
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
