//! Provides struct for representing a constraint in an optimization problem
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::optimize::variable::{Variable, VariableBuilder};

/// Represents a linear constraint in an optimization problem
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Represents an equality constraint, where `terms` = `equals`
    Equality {
        /// Linear terms which are added together, see [`ConstraintTerm`] for more 
        terms: Vec<ConstraintTerm>,
        /// The right hand side of the equality constraint
        equals: f64,
    },
    /// Represents an inequality constraint,
    Inequality {
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
    ///     .wrap(); // This wraps the variable in a Rc<RefCell<>>
    /// let y = VariableBuilder::default()
    ///     .id("y")
    ///     .lower_bound(3.0)
    ///     .upper_bound(7.0)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in a Rc<RefCell<>>
    /// // Create a constraint representing 3*x + 2*y = 6
    /// let new_constraint = Constraint::new_equality(&[x,y], &[3.0,2.0], 6.);
    /// ```
    pub fn new_equality(
        variables: &[Rc<RefCell<Variable>>],
        coefficients: &[f64],
        equals: f64,
    ) -> Self {
        Constraint::Equality {
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
    ///     .wrap(); // This wraps the variable in a Rc<RefCell<>>
    /// let y = VariableBuilder::default()
    ///     .id("y")
    ///     .lower_bound(3.0)
    ///     .upper_bound(7.0)
    ///     .build()
    ///     .unwrap()
    ///     .wrap(); // This wraps the variable in a Rc<RefCell<>>
    /// // represents the inequality 2 <= 3*x + 2*y <= 6
    /// let new_constraint = Constraint::new_inequality(&[x,y], &[3.0,2.0], 2., 6.);
    /// ```
    pub fn new_inequality(
        variables: &[Rc<RefCell<Variable>>],
        coefficients: &[f64],
        lower_bound: f64,
        upper_bound: f64,
    ) -> Self {
        Constraint::Inequality {
            terms: Constraint::zip_into_terms(variables, coefficients),
            lower_bound, 
            upper_bound,
        }
    }
    
    /// Wrap the constraint in a Rc<RefCell<>>
    pub fn wrap(self)-> Rc<RefCell<Self>>{
        Rc::new(RefCell::new(self))
    }

    /// Take a slice of variable references, and a slice of coefficients and zip
    /// them together into a vec of ConstraintTerms
    fn zip_into_terms(
        variables: &[Rc<RefCell<Variable>>],
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
            Constraint::Equality { terms, equals } => {
                format!("{} = {}", Self::terms_to_string(terms), equals)
            }
            Constraint::Inequality {
                terms,
                lower_bound,
                upper_bound,
            } => {
                format!(
                    "{} <= {} <= {}",
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
    variable: Rc<RefCell<Variable>>,
    /// The coefficient for the variable
    coefficient: f64,
}

impl Display for ConstraintTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}*{}", self.coefficient, self.variable.borrow().id)
    }
}
