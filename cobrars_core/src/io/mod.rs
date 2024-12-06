//! Module for reading and writing Models
pub mod gpr_parse;
pub mod json;

pub enum IoError {
    FileNotFound,
    DeserializeError,
}
