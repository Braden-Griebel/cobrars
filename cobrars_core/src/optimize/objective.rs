//! Provides struct for representing an optimization problem's object

use std::sync::{Arc, RwLock};

use crate::optimize::variable::Variable;

/// Represents the Objective of an optimization problem
#[derive(Debug, Clone)]
pub struct Objective {
    /// Terms included in the objective (See [`ObjectiveTerm`]
    terms: Vec<ObjectiveTerm>,
    /// Sense of the objective (maximize, or minimize), see [`ObjectiveSense`]
    sense: ObjectiveSense,
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
    pub fn add_linear_term(&mut self, variable: Arc<RwLock<Variable>>, coefficient: f64) {
        self.terms.push(ObjectiveTerm::Linear {
            var: variable,
            coef: coefficient,
        });
    }

    /// Add a new Quadratic term to the objective
    pub fn add_quadratic_term(
        &mut self,
        variable1: Arc<RwLock<Variable>>,
        variable2: Arc<RwLock<Variable>>,
        coefficient: f64,
    ) {
        self.terms.push(ObjectiveTerm::Quadratic {
            var1: variable1,
            var2: variable2,
            coef: coefficient,
        });
    }

    /// Add a series of linear terms to the objective function
    pub fn add_linear_terms(&mut self, variables: &[Arc<RwLock<Variable>>], coefficient: &[f64]) {
        self.terms
            .extend(Objective::zip_linear_terms(variables, coefficient));
    }

    /// Add a series of quadratic terms to the objective function
    pub fn add_quadratic_terms(
        &mut self,
        variables1: &[Arc<RwLock<Variable>>],
        variables2: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
    ) {
        self.terms.extend(Objective::zip_quadratic_terms(
            variables1,
            variables2,
            coefficients,
        ));
    }

    /// Zip together slice of variable references with coefficients to create linear terms
    fn zip_linear_terms(
        variables: &[Arc<RwLock<Variable>>],
        coefficient: &[f64],
    ) -> Vec<ObjectiveTerm> {
        variables
            .iter()
            .zip(coefficient)
            .map(|(var, coef)| ObjectiveTerm::new_linear(var.clone(), *coef))
            .collect()
    }

    /// Zip together two slices of variable and references with coefficients to create quadratic
    /// terms
    fn zip_quadratic_terms(
        variables1: &[Arc<RwLock<Variable>>],
        variables2: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
    ) -> Vec<ObjectiveTerm> {
        variables1
            .iter()
            .zip(variables2)
            .zip(coefficients)
            .map(|((v1, v2), c)| ObjectiveTerm::new_quadratic(v1.clone(), v2.clone(), *c))
            .collect()
    }
}

/// Represents the sense of the objective, whether it should be maximized or minimized
#[derive(Debug, Clone, Copy)]
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
        var1: Arc<RwLock<Variable>>,
        /// Second variable in the objective term
        var2: Arc<RwLock<Variable>>,
        /// Coefficient for quadratic term
        coef: f64,
    },
    /// A linear term in the objective
    Linear {
        /// Variable in objective term
        var: Arc<RwLock<Variable>>,
        /// Coefficient for linear term
        coef: f64,
    },
}

impl ObjectiveTerm {
    /// Create a new quadratic objective term
    pub fn new_quadratic(
        var1: Arc<RwLock<Variable>>,
        var2: Arc<RwLock<Variable>>,
        coef: f64,
    ) -> Self {
        ObjectiveTerm::Quadratic { var1, var2, coef }
    }

    /// Create a new linear objective term
    pub fn new_linear(var: Arc<RwLock<Variable>>, coef: f64) -> Self {
        ObjectiveTerm::Linear { var, coef }
    }
}

// endregion Objective Terms
