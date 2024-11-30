//! Module providing Token struct for lexing

/// Represents Tokens in GPR
#[derive(Debug, PartialEq, Clone, Eq)]
pub(super) enum Token {
    Identifier(String),
    And,
    Or,
    LeftParen,
    RightParen,
    Eof,
}