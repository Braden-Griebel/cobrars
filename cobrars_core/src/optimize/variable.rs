//! Module providing representation of optimization problem variables
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock};

use derive_builder::Builder;

use crate::configuration::CONFIGURATION;

/// Represents a bounded variable in an optimization problem
#[derive(Builder, Debug, Clone)]
pub struct Variable {
    /// ID used to identify the variable
    #[builder(setter(into))]
    pub id: String,
    /// Optional variable name
    #[builder(setter(into, strip_option), default = "None")]
    pub name: Option<String>,
    /// Type of variable, see [`VariableType`] for more information
    #[builder(default = "VariableType::Continuous")]
    pub variable_type: VariableType,
    /// Index of the variable (used internally)
    #[builder(default = "0")]
    pub (crate) index: usize,
    /// Lower bound for the variable, the smallest value it can take
    #[builder(default = "0.0")]
    pub lower_bound: f64,
    /// Upper bound for the variable, the largest value it can take
    #[builder(default = "CONFIGURATION.read().unwrap().upper_bound")]
    pub upper_bound: f64,
}

impl Variable {
    /// Create a new variable
    ///
    /// # Parameters
    /// - `id`: Variable identifier string
    /// - `name`: Variable name (optional)
    /// - `variable_type`: Type of variable, Continuous, Integer, or Binary (see [`VariableType`])
    /// - `index`: Index of the variable, only meaningful when part of a problem
    /// - `lower_bound`: Lower bound of the variable
    /// - `upper_bound`: Upper bound of the variable
    ///
    /// # Returns
    /// A new variable, wrapped in Arc<RwLock<>>
    fn new(
        id: String,
        name: Option<String>,
        variable_type: VariableType,
        index: usize,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Arc<RwLock<Variable>> {
        Arc::new(RwLock::new(Variable {
            id,
            name,
            variable_type,
            index,
            lower_bound,
            upper_bound,
        }))
    }
    
    /// Returns a wrapped reference to the variable which can then be used when constructing 
    /// constraints
    pub fn wrap(self) -> Arc<RwLock<Variable>> {
        Arc::new(RwLock::new(self))
    }

    pub(crate) fn get_id(&self) -> String {
        self.id.clone()
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.id, self.variable_type)
    }
}

/// Represents the type of variable in an optimization problem
///
/// # Notes:
/// Not all variable types are supported for all solvers, currently Clarabel and OSQP only supports
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
