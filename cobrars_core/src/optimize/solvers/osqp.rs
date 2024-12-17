//! Solver interface for OSQP solver

use crate::optimize::objective::ObjectiveSense;
use crate::optimize::ProblemSolution;
use crate::optimize::solvers::{Solver, SolverError};

#[derive(Clone, Debug)]
pub struct OsqpSolver {}

impl Solver for OsqpSolver {
    fn quadratic_objective_capable(&self) -> bool {
        todo!()
    }

    fn integer_variable_capable(&self) -> bool {
        todo!()
    }

    fn binary_variable_capable(&self) -> bool {
        todo!()
    }

    fn add_continuous_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn add_binary_variable(&mut self, id: &str) -> Result<(), SolverError> {
        todo!()
    }

    fn add_integer_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn add_equality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        equals: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn add_inequality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn add_linear_objective_term(
        &mut self,
        variable_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn add_quadratic_objective_term(
        &mut self,
        variable1_id: &str,
        variable2_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError> {
        todo!()
    }

    fn set_objective_sense(&mut self, objective_sense: ObjectiveSense) -> Result<(), SolverError> {
        todo!()
    }

    fn clear_objective(&mut self) -> Result<(), SolverError> {
        todo!()
    }

    fn solve(&mut self) -> Result<ProblemSolution, SolverError> {
        todo!()
    }
}
