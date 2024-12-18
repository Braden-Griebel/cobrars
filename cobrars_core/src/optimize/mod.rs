//! Module for constructing and solving optimization problems

pub mod constraint;
pub mod objective;
pub mod problem;
pub mod solvers;
pub mod variable;

use indexmap::IndexMap;

/// Struct representing the solution to an optimization problem
pub struct ProblemSolution {
    /// The status of the optimization problem, representing if the optimization was
    /// completed successfully
    pub status: OptimizationStatus,
    /// Optimized value of the objective
    ///
    /// Some(f64) if the optimization was completed successfully, None otherwise
    pub objective_value: Option<f64>,
    /// Values of the variables at the optimum,
    ///
    /// Some(IndexMap), keyed by variable id, with values corresponding to variable
    /// values at optimum if the problem could be solved, None otherwise
    pub variable_values: Option<IndexMap<String, f64>>,
    /// Values of the dual variables at the optimum
    ///
    /// Some(IndexMap), keyed by constraint id, with values corresponding to dual
    /// variable values at optimum if the problem could be solved, and the solver
    /// supports retrieving the dual values, None otherwise
    pub dual_values: Option<IndexMap<String, f64>>,
}

/// Status of an optimization problem
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OptimizationStatus {
    /// Problem has not yet attempted to be optimized
    Unoptimized,
    /// Problem has been optimized
    Optimal,
    /// Problem can't be optimized because objective value is not bounded
    Unbounded,
    /// Problem can't be solved because it is infeasible (conflicting constraints)
    Infeasible,
    /// An approximate solution has been found
    AlmostOptimal,
    /// A numerical error occurred during solving
    NumericalError,
    /// The solver hit the maximum allowed iterations, or max time, or made insufficient progress
    SolverHalted,
}
