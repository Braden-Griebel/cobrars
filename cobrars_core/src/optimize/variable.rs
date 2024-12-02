//! Module providing representation of optimization problem variables
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Variable {
    id: String,
    name: Option<String>,
    variable_type: VariableType,
    index: usize,
}

impl Variable {
    /// Create a new variable
    ///
    /// # Parameters
    /// - id: Variable identifier string
    /// - name: Variable name (optional)
    /// - variable_type: Type of variable, Continuous, Integer, or Binary (see [`VariableType`]
    /// - index: Index of the variable, only meaningful when part of a problem
    ///
    /// # Returns
    /// A new variable, wrapped in Rc<RefCell<>>
    fn new(
        id: String,
        name: Option<String>,
        variable_type: VariableType,
        index: usize,
    ) -> Rc<RefCell<Variable>> {
        Rc::new(RefCell::new(Variable {
            id,
            name,
            variable_type,
            index,
        }))
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "{}:{}", name, self.variable_type),
            None => write!(f, "{}:{}", self.id, self.variable_type),
        }
    }
}

/// Represents the type of variable in an optimization problem
///
/// # Notes:
/// Not all variable types are supported for all solvers, currently Clarabel only supports
/// Continuous variables, while Russcip supports all types
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum VariableType {
    /// Continuous variable
    Continuous,
    /// Integer variable
    Integer,
    /// Binary Variable
    Binary,
}

impl Display for VariableType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableType::Continuous => write!(f, "CONTINUOUS"),
            VariableType::Integer => write!(f, "INTEGER"),
            VariableType::Binary => write!(f, "BINARY"),
        }
    }
}
