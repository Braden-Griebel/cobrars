//! Provides struct for representing an optimization problem's object
use crate::optimize::variable::Variable;

/// Represents the Objective of an optimization problem
#[derive(Debug, Clone)]
pub struct Objective {
    /// Terms included in the objective (See [`ObjectiveTerm`]
    pub(crate) terms: Vec<ObjectiveTerm>,
    /// Sense of the objective (maximize, or minimize), see [`ObjectiveSense`]
    pub(crate) sense: ObjectiveSense,
}

impl Objective {
    /// Create a new empty objective, with a given sense
    pub fn new(sense: ObjectiveSense) -> Self {
        Self {
            terms: Vec::new(),
            sense,
        }
    }

    /// Create a new empty maximization objective
    pub fn new_maximize() -> Self {
        Self::new(ObjectiveSense::Maximize)
    }

    /// Create a new empty minimization objective
    pub fn new_minimize() -> Self {
        Self::new(ObjectiveSense::Minimize)
    }

    /// Change the sense of the objective
    pub fn set_sense(&mut self, sense: ObjectiveSense) {
        self.sense = sense;
    }

    /// Add a new term to the objective
    pub fn add_term(&mut self, term: ObjectiveTerm) {
        self.terms.push(term);
    }

    /// Add a new Linear term to the objective
    pub fn add_linear_term(&mut self, variable: &str, coefficient: f64) {
        self.terms.push(ObjectiveTerm::Linear {
            var: variable.to_string(),
            coef: coefficient,
        });
    }

    /// Add a new Quadratic term to the objective
    pub fn add_quadratic_term(
        &mut self,
        variable1: &str,
        variable2: &str,
        coefficient: f64,
    ) {
        self.terms.push(ObjectiveTerm::Quadratic {
            var1: variable1.to_string(),
            var2: variable2.to_string(),
            coef: coefficient,
        });
    }

    /// Add a series of linear terms to the objective function
    pub fn add_linear_terms(&mut self, variables: &[&str], coefficient: &[f64]) {
        self.terms
            .extend(Objective::zip_linear_terms(variables, coefficient));
    }

    /// Add a series of quadratic terms to the objective function
    pub fn add_quadratic_terms(
        &mut self,
        variables1: &[&str],
        variables2: &[&str],
        coefficients: &[f64],
    ) {
        self.terms.extend(Objective::zip_quadratic_terms(
            variables1,
            variables2,
            coefficients,
        ));
    }

    /// Checks if the objective term contains quadratic terms
    pub fn contains_quadratic(&self) -> bool {
        for t in &self.terms {
            if let ObjectiveTerm::Quadratic { .. } = t {
                return true;
            }
        }
        false
    }

    /// Empty the objective of all terms
    pub fn remove_all_terms(&mut self) {
        self.terms = Vec::new();
    }

    /// Remove any terms which include a variable with a given `id`
    pub fn remove_terms_with_variable(&mut self, id: &str) {
        self.terms = self
            .terms
            .drain(..)
            .filter(|t| match t {
                ObjectiveTerm::Linear { var, .. } => var != id,
                ObjectiveTerm::Quadratic { var1, var2, .. } => {
                    !((var1 == id) || (var2 == id))
                }
            })
            .collect();
    }

    /// Zip together slice of variable references with coefficients to create linear terms
    fn zip_linear_terms(
        variables: &[&str],
        coefficient: &[f64],
    ) -> Vec<ObjectiveTerm> {
        variables
            .iter()
            .zip(coefficient)
            .map(|(var, coef)| ObjectiveTerm::new_linear(var, *coef))
            .collect()
    }

    /// Zip together two slices of variable and references with coefficients to create quadratic
    /// terms
    fn zip_quadratic_terms(
        variables1: &[&str],
        variables2: &[&str],
        coefficients: &[f64],
    ) -> Vec<ObjectiveTerm> {
        variables1
            .iter()
            .zip(variables2)
            .zip(coefficients)
            .map(|((v1, v2), c)| ObjectiveTerm::new_quadratic(v1, v2, *c))
            .collect()
    }
}

/// Represents the sense of the objective, whether it should be maximized or minimized
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectiveSense {
    /// The objective should be minimized
    Minimize,
    /// The objective should be maximized
    Maximize,
}

// region Objective Terms
/// A term in the objective
#[derive(Debug, Clone)]
pub enum ObjectiveTerm {
    /// A quadratic term in the objective
    Quadratic {
        /// First variable in the objective term
        var1: String,
        /// Second variable in the objective term
        var2: String,
        /// Coefficient for quadratic term
        coef: f64,
    },
    /// A linear term in the objective
    Linear {
        /// Variable in objective term
        var: String,
        /// Coefficient for linear term
        coef: f64,
    },
}

impl ObjectiveTerm {
    /// Create a new quadratic objective term
    pub fn new_quadratic(
        var1: &str,
        var2: &str,
        coef: f64,
    ) -> Self {
        ObjectiveTerm::Quadratic { var1: var1.to_string(), var2: var2.to_string(), coef }
    }

    /// Create a new linear objective term
    pub fn new_linear(var: &str, coef: f64) -> Self {
        ObjectiveTerm::Linear { var: var.to_string(), coef }
    }
}

// endregion Objective Terms
