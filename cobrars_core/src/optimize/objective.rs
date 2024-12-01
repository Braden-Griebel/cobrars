//! Provides struct for representing an optimization problem's object

use std::rc::Rc;
use crate::optimize::variable::Variable;

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
pub enum ObjectiveTerm{
    Quadratic{
        var1: Rc<Variable>,
        var1_coefficient: f64,
        var2: Rc<Variable>,
        var2_coefficient: f64,
    },
    Linear {
        var: Rc<Variable>,

    }
}
// endregion Objective Terms