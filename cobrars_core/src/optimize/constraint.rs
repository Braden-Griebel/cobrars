//! Provides struct for representing a constraint in an optimization problem

use std::env::var;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock};

use crate::optimize::variable::{Variable, VariableBuilder};

/// Represents a linear constraint in an optimization problem
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Represents an equality constraint, where `terms` = `equals`
    Equality {
        /// ID of the constraint
        id: String,
        /// Linear terms which are added together, see [`ConstraintTerm`] for more 
        terms: Vec<ConstraintTerm>,
        /// The right hand side of the equality constraint
        equals: f64,
    },
    /// Represents an inequality constraint,
    Inequality {
        /// ID of the constraint
        id: String,
        /// Linear terms which are added together, see [`ConstraintTerm`] for more 
        terms: Vec<ConstraintTerm>,
        /// The lowest value the sum of the terms can take
        lower_bound: f64,
        /// The highest value the sum of the terms can take
        upper_bound: f64,
    },
}

impl Constraint {
    /// Create a new equality constraint
    ///
    /// # Parameters
    /// - `variables`: A slice of wrapped variables
    /// - `coefficients`: A slice of coefficients for the variables
    /// - `equals`: The right hand side of the equality
    ///
    /// # Returns
    /// A new equality constraint
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::optimize::constraint::Constraint;
    /// use cobrars_core::optimize::variable::VariableBuilder;
    /// let x = VariableBuilder::default()
    ///     .id("x")
    ///     .lower_bound(0.0)
    ///     .upper_bound(20.)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in a Arc<RwLock<>>
    /// let y = VariableBuilder::default()
    ///     .id("y")
    ///     .lower_bound(3.0)
    ///     .upper_bound(7.0)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in an Arc<RwLock<>>
    /// // Create a constraint representing 3*x + 2*y = 6
    /// let new_constraint = Constraint::new_equality("Example Equality Constraint", &[x,y], &[3.0,2.0], 6.);
    /// ```
    pub fn new_equality(
        id: &str,
        variables: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
        equals: f64,
    ) -> Self {
        Constraint::Equality {
            id: id.to_string(),
            terms: Constraint::zip_into_terms(variables, coefficients),
            equals,
        }
    }
    
    /// Create a new inequality constraint
    ///
    /// # Parameters
    /// - `variables`: A slice of wrapped variables
    /// - `coefficients`: A slice of coefficients for the variables
    /// - `lower_bound`: The lowest value the constraint can take
    /// - `upper_bound`: The highest value the constraint can take
    ///
    /// # Returns
    /// A new inequality constraint
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::optimize::constraint::Constraint;
    /// use cobrars_core::optimize::variable::VariableBuilder;
    /// let x = VariableBuilder::default()
    ///     .id("x")
    ///     .lower_bound(0.0)
    ///     .upper_bound(20.)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in a Arc<RwLock<>>
    /// let y = VariableBuilder::default()
    ///     .id("y")
    ///     .lower_bound(3.0)
    ///     .upper_bound(7.0)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in a Arc<RwLock<>>
    /// // represents the inequality 2 <= 3*x + 2*y <= 6
    /// let new_constraint = Constraint::new_inequality("Example Inequality Account", &[x,y], &[3.0,2.0], 2., 6.);
    /// ```
    pub fn new_inequality(
        id: &str,
        variables: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
        lower_bound: f64,
        upper_bound: f64,
    ) -> Self {
        Constraint::Inequality {
            id: id.to_string(),
            terms: Constraint::zip_into_terms(variables, coefficients),
            lower_bound, 
            upper_bound,
        }
    }
    
    /// Wrap the constraint in an Arc<RwLock<>>
    pub fn wrap(self)-> Arc<RwLock<Self>>{
        Arc::new(RwLock::new(self))
    }
    
    /// Get a vec of variables in the constraint
    pub(crate) fn get_variables(&self) -> Vec<Arc<RwLock<Variable>>> {
        let mut variables = Vec::new();
        match self {
            Constraint::Equality { terms, .. } => {
                for term in terms {
                    variables.push(term.variable.clone());
                }
            }
            Constraint::Inequality { terms, .. } => {
                for term in terms {
                    variables.push(term.variable.clone());
                }
            }
        }
        variables
    }
    
    /// Remove any terms in the constraint which includes a given variable (passed by id)
    pub fn remove_variable(&mut self, variable_id: &str){
        match self {
            Constraint::Equality { terms, .. } | Constraint::Inequality {terms, ..}=> {
                *terms = terms.drain(..).filter(|t| {
                    t.variable.read().unwrap().id != variable_id
                }).collect()
            }
        }
    }
    

    /// Take a slice of variable references, and a slice of coefficients and zip
    /// them together into a vec of ConstraintTerms
    fn zip_into_terms(
        variables: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
    ) -> Vec<ConstraintTerm> {
        variables
            .iter()
            .zip(coefficients)
            .map(|(var, coef)| ConstraintTerm {
                variable: var.clone(),
                coefficient: coef.clone(),
            })
            .collect()
    }

    /// Create a string representation of the terms in the Constraint
    fn constraint_to_string(&self) -> String {
        match self {
            Constraint::Equality {id, terms, equals } => {
                format!("{}: {} = {}", id, Self::terms_to_string(terms), equals)
            }
            Constraint::Inequality {
                id,
                terms,
                lower_bound,
                upper_bound,
            } => {
                format!(
                    "{}: {} <= {} <= {}",
                    id,
                    lower_bound,
                    Self::terms_to_string(terms),
                    upper_bound
                )
            }
        }
    }

    /// Convert a vector of terms into a String representation
    fn terms_to_string(terms: &[ConstraintTerm]) -> String {
        let mut str_rep = String::new();
        for t in &terms[..terms.len() - 1] {
            str_rep.push_str(format!("{} + ", t).as_str());
        }
        str_rep.push_str(format!("{}", terms.last().unwrap()).as_str());
        str_rep
    }
    
    /// Get the id of the constraint
    pub fn get_id(&self)->String {
        match self {
            Constraint::Equality { id, .. } => id.to_string(),
            Constraint::Inequality { id, .. } => id.to_string(),
        }
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constraint_to_string())
    }
}

/// Represents a single term in a constraint, specifically
/// represents the multiplication of the `variable` by the `coefficient`
#[derive(Debug, Clone)]
pub struct ConstraintTerm {
    /// A reference to a [`Variable`]
    variable: Arc<RwLock<Variable>>,
    /// The coefficient for the variable
    coefficient: f64,
}

impl Display for ConstraintTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}*{}", self.coefficient, self.variable.read().unwrap().id)
    }
}
