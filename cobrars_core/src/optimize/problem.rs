//! Provides struct representing an optimization problem
use crate::optimize::constraint::Constraint;
use crate::optimize::objective::Objective;
use crate::optimize::variable::Variable;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

/// An optimization problem
#[derive(Debug, Clone)]
pub struct Problem {
    /// Objective to optimize
    objective: Objective,
    /// Variables of the optimization problem
    variables: IndexMap<String, Rc<RefCell<Variable>>>,
    /// Constraints of the optimization problem
    constraints: IndexMap<String, Rc<RefCell<Constraint>>>,
    /// Current status of the optimization problem
    status: OptimizationStatus,
    /// Values of the optimized variables. Will be None before optimization,
    variable_values: Option<IndexMap<Variable, f64>>,
    /// Type of problem
    problem_type: ProblemType,
    /// Current number of variables in the model
    num_variables: usize,
    /// Current number of constraints in the model
    num_constraints: usize,
}

impl Problem {
    // pub fn new() -> Self {
    //     
    // }
}

/// Status of an optimization problem
#[derive(Copy, Clone, Debug)]
pub enum OptimizationStatus {
    /// Problem has not yet attempted to be optimized
    Unoptimized,
    /// Problem has been optimized
    Optimal,
    /// Problem can't be optimized because objective value is not bounded
    Unbounded,
    /// Problem can't be solved because it is infeasible (conflicting constraints)
    Infeasible,
}

/// Types of optimization problems
#[derive(Clone, Debug)]
pub enum ProblemType {
    /// Problem with linear objectives and constraints, and continuous variables
    LinearContinuous,
    /// Problem with quadratic objective, linear constraints, and continuous variables
    QuadraticContinuous,
    /// Problem with linear objective and constraints, with integer and continuous variables
    LinearMixedInteger,
}
