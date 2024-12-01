//! Provides struct for representing a constraint in an optimization problem
use std::rc::Rc;
use std::fmt::{Display, Formatter};
use crate::optimize::variable::Variable;

/// Represents a linear constraint in an optimization problem
pub struct Constraint {
    /// The left hand side of the constraint, a vec of [`ConstraintTerm`]s
    lhs: Vec<ConstraintTerm>,
    /// The right hand side of the constraint, a single number
    rhs: f64,
    /// The type of the constraint, see [`ConstraintType`]
    constraint_type: ConstraintType,
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.lhs.is_empty() {
            return write!(f, "Empty Constraint")
        }
        let mut string_rep = String::new();
        // Iterate through all but the last term (so the + are correct)
        for term in &self.lhs[..self.lhs.len() - 1] {
            string_rep.push_str(format!("{} + ", term).as_str());
        }
        string_rep.push_str(format!("{}", self.lhs[self.lhs.len()-1]).as_str());
        string_rep.push_str(format!(" {} ", self.constraint_type).as_str());
        string_rep.push_str(format!("{}", self.rhs).as_str());
        write!(f, "{}", string_rep)
    }
}


/// Enum representing the different types of constraints
pub enum ConstraintType {
    /// An equality constraint, where the lhs equals the rhs
    Equality,
    /// A Greater than or Equal to constraint, >=
    GreaterThanOrEqual,
    /// A Less than or Equal to constriant, <=
    LessThanOrEqual,
}

impl Display for ConstraintType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintType::Equality => write!(f, "="),
            ConstraintType::GreaterThanOrEqual => write!(f, ">="),
            ConstraintType::LessThanOrEqual => write!(f, "<="),
        }
    }
}

/// Struct representing a single term on the lhs of a constraint
pub struct ConstraintTerm {
    /// The variable included in the term
    variable: Rc<Variable>,
    /// The coefficient of the variable in the expression
    coefficient: f64,
}

impl Display for ConstraintTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}*{}", self.coefficient, self.variable)
    }
}