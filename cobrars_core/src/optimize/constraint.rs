//! Provides struct for representing a constraint in an optimization problem

use std::cell::RefCell;
use crate::optimize::variable::Variable;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// Represents a linear constraint in an optimization problem
pub enum Constraint {
    /// Represents an equality constraint, where `terms` = `equals`
    Equality{
        /// Linear terms 
        terms: Vec<ConstraintTerm>,
        equals: f64,        
    },
    Inequality {
        terms: Vec<ConstraintTerm>,
        lower_bound: f64,
        upper_bound: f64,
    }
}

impl Constraint {
    /// Create a string representation of the terms in the Constraint
    fn constraint_to_string(&self) -> String {
        match self {
            Constraint::Equality{terms, equals} => {
                format!("{} = {}", Self::terms_to_string(terms), equals)
            }
            Constraint::Inequality{terms, lower_bound, upper_bound} => {
                format!("{} <= {} <= {}", lower_bound, Self::terms_to_string(terms), upper_bound)
            }
        }
    }
    
    /// Convert a vector of terms into a String representation
    fn terms_to_string(terms: &[ConstraintTerm])-> String {
        let mut str_rep = String::new();
        for t in &terms[..terms.len()-1] {
            str_rep.push_str(format!("{} + ", t).as_str());
        };
        str_rep.push_str(format!("{}", terms.last().unwrap()).as_str());
        str_rep
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.constraint_to_string())
    }
}

pub struct ConstraintTerm {
    variable: Rc<RefCell<Variable>>,
    coef: f64,
}

impl Display for ConstraintTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}*{}", self.coef, self.variable.borrow().id)
    }
}