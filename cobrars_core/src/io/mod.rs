pub mod gpr_parse;
pub mod json;

pub enum IoError {
    FileNotFound,
    DeserializeError,
}
