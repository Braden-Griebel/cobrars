//! Provides struct for representing an optimization problem's object

use crate::optimize::variable::Variable;
use std::cell::RefCell;
use std::rc::Rc;

/// Represents the Objective of an optimization problem
pub struct Objective {
    /// Terms included in the objective (See [`ObjectiveTerm`]
    terms: Vec<ObjectiveTerm>,
    /// Sense of the objective (maximize, or minimize), see [`ObjectiveSense`]
    sense: ObjectiveSense,
}

/// Represents the sense of the objective, whether it should be maximized or minimized
pub enum ObjectiveSense {
    /// The objective should be minimized
    Minimize,
    /// The objective should be maximized
    Maximize,
}

// region Objective Terms
/// A term in the objective
pub enum ObjectiveTerm {
    /// A quadratic term in the objective
    Quadratic {
        /// First variable in the objective term
        var1: Rc<RefCell<Variable>>,
        /// Second variable in the objective term
        var2: Rc<RefCell<Variable>>,
        /// Coefficient for quadratic term
        coef: f64,
    },
    /// A linear term in the objective
    Linear {
        /// Variable in objective term
        var: Rc<RefCell<Variable>>,
        /// Coefficient for linear term
        coef: f64,
    },
}

impl ObjectiveTerm {
    pub fn new_quadratic(
        var1: Rc<RefCell<Variable>>,
        var2: Rc<RefCell<Variable>>,
        coef: f64,
    ) -> Self {
        ObjectiveTerm::Quadratic { var1, var2, coef }
    }

    pub fn new_linear(var: Rc<RefCell<Variable>>, coef: f64) -> Self {
        ObjectiveTerm::Linear { var, coef }
    }
}

// endregion Objective Terms
