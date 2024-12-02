//! Module providing Token struct for lexing

/// Represents Tokens in GPR
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub(super) enum Token {
    Identifier(String),
    And,
    Or,
    Not,
    LeftParen,
    RightParen,
    Eof,
}
