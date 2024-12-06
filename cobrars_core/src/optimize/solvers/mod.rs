//! Provides interface to backend solvers

use crate::optimize::objective::{Objective, ObjectiveSense};
use crate::optimize::ProblemSolution;
use std::fmt::Debug;
use thiserror::Error;

#[cfg(feature = "scip")]
pub mod scip;

#[cfg(feature = "osqp")]
pub mod osqp;

pub mod clarabel;

/// Trait for structs implementing a solver backend interface
pub trait Solver: Debug + Clone {
    // region capabilities
    /// Returns whether the solver can handle a quadratic objective
    fn quadratic_objective_capable(&self) -> bool;
    /// Returns whether the solver can handle integer variables
    fn integer_variable_capable(&self) -> bool;
    /// Returns whether the solver can handle binary variables
    fn binary_variable_capable(&self) -> bool;
    // endregion capabilities
    // region variables
    /// Add a continuous variable to the solver
    ///
    /// # Note
    /// All solvers should be capable of this
    fn add_continuous_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError>;
    /// Add a binary variable to the solver
    ///
    /// # Note
    /// This shouldn't be called on solvers which aren't capable of having binary variables,
    /// but it should return an error in those cases for safety
    fn add_binary_variable(&mut self, id: &str) -> Result<(), SolverError>;
    /// Add an integer variable to the solver
    ///
    /// # Note
    /// This shouldn't be called on solvers which aren't capable of having binary variables,
    /// but it should return an error in those cases for safety
    fn add_integer_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError>;
    // endregion variables
    // region constraints
    /// Add an equality constraint to the problem
    fn add_equality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        equals: f64,
    ) -> Result<(), SolverError>;
    /// Add an inequality constraint to the problem
    fn add_inequality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError>;
    // endregion constraints
    // region objective
    /// Add a linear term to the objective
    fn add_linear_objective_term(
        &mut self,
        variable_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError>;
    /// Add a quadratic term to the objective
    fn add_quadratic_objective_term(
        &mut self,
        variable1_id: &str,
        variable2_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError>;

    /// Set the objective sense
    fn set_objective_sense(&mut self, objective_sense: ObjectiveSense) -> Result<(), SolverError>;
    /// Remove all previous terms from the objective
    fn clear_objective(&mut self) -> Result<(), SolverError>;
    // endregion objective
    // region solve
    fn solve(&mut self) -> Result<ProblemSolution, SolverError>;
    // endregion solve
}

/// Possible solver error states
///
/// # Note
/// The solver can fail to find a solution without having an error, for example
/// if the problem is inconsistent then the solver will return a ProblemSolution
/// with an Infeasible OptimizationStatus
#[derive(Error, Debug, Clone)]
pub enum SolverError {}
