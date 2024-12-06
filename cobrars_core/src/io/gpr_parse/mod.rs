//! Module for parsing Gene Protein Reaction strings into AST values

use crate::io::gpr_parse::lexer::LexerError;
use crate::io::gpr_parse::parser::ParseError;
use crate::metabolic_model::gene::{Gene};
use crate::metabolic_model::model::{Gpr};
use indexmap::IndexMap;
use std::sync::{Arc, RwLock};
use thiserror::Error;

mod lexer;
pub mod parser;
mod token;

/// Parse a Gene Protein Reaction string into a GPR Tree
///
/// # Parameters
/// - `input`: &str representing the gene protein reaction rule
/// - `gene_map`: map of gene id strings to genes (wrapped in Arc<RwLock<>>)
///
/// # Returns
/// Parse result which is
/// - `Ok`: Includes a tuple of the root node of the GPR tree, and the updated `gene_map`.
/// - `Err`: Returns the GprParseError describing the issue with the GPR rule which
///     was being parsed.
///
/// # Examples
/// ```rust
/// use indexmap::IndexMap;
/// use cobrars_core::io::gpr_parse::parse_gpr;
/// let gpr: &str = "Rv0001 and Rv0002";
/// let mut gene_map = IndexMap::new();
/// let gpr_tree = parse_gpr(gpr, &mut gene_map).unwrap();
/// ```
pub fn parse_gpr(
    input: &str,
    gene_map: &mut IndexMap<String, Gene>,
) -> Result<Gpr, GprParseError> {
    // Start by creating a lexer
    let mut lexer = lexer::Lexer::new(input);
    // Convert the GPR string into tokens
    let tokens = lexer.lex()?;

    // Now parse those tokens into a GPR tree
    // Create the parser
    let mut parser = parser::GPRParser::new(tokens, gene_map);
    // Parse the expression
    let gpr = parser.parse()?;
    Ok(gpr)
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
    use crate::metabolic_model::gene::{Gene, GeneActivity};
    use crate::metabolic_model::model::{Gpr, GprOperation};
    use indexmap::IndexMap;
    use std::sync::{Arc, RwLock};

    #[test]
    fn test_parse_gpr() {
        let gpr = "Rv0001 and (Rv0002 or Rv0003)";
        let mut gene_map: IndexMap<String, Gene> = IndexMap::new();
        gene_map.insert(
            "Rv0001".to_string(),
            Gene::new(
                "Rv0001".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ),
        );
        gene_map.insert(
            "Rv0002".to_string(),
            Gene::new(
                "Rv0002".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ),
        );
        gene_map.insert(
            "Rv0003".to_string(),
            Gene::new(
                "Rv0003".to_string(),
                None,
                GeneActivity::Active,
                None,
                None,
            ),
        );
        let gpr_tree = parse_gpr(gpr, &mut gene_map).unwrap();
        match gpr_tree {
            Gpr::Operation(op) => match op {
                GprOperation::And { left, right } => {
                    match *left {
                        Gpr::GeneNode(g) => {
                            if g != "Rv0001" {
                                panic!("Incorrect Parse")
                            }
                        }
                        _ => panic!("Incorrect Parse"),
                    }
                    match *right {
                        Gpr::Operation(GprOperation::Or { left, right }) => {
                            match *left {
                                Gpr::GeneNode(g) => {
                                    if g != "Rv0002" {
                                        panic!("Incorrect Parse")
                                    }
                                }
                                _ => panic!("Incorrect Parse"),
                            }
                            match *right {
                                Gpr::GeneNode(g) => {
                                    if g != "Rv0003" {
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
            Gpr::GeneNode(_) => {
                panic!("Incorrect gpr parse")
            }
        }
    }
}
