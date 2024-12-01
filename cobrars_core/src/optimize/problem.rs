//! Provides struct representing an optimization problem
use std::rc::Rc;
use indexmap::IndexMap;
use crate::optimize::objective::Objective;
use crate::optimize::variable::Variable;
use crate::optimize::constraint::Constraint;

pub struct Problem {
    objective: Objective,
    variables: IndexMap<String, Rc<Variable>>,
    constraints: IndexMap<String, Constraint>,
    status: OptimizationStatus,
    variable_values: Option<IndexMap<Variable, f64>>
}


pub enum OptimizationStatus {
    UnOptimized,
    Optimal,
    Unbounded,
    Infeasible,
}