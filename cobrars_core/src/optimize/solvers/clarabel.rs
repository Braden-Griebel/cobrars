//! Implements a solver interface for Clarabel
use indexmap::IndexMap;
use nalgebra_sparse::coo::CooMatrix;

use crate::optimize::objective::ObjectiveSense;
use crate::optimize::solvers::{Solver, SolverError};
use crate::optimize::ProblemSolution;

/// Solver backend to interface with Clarabel
///
/// #### Note
/// Currently, this interface essentially recreates the problem each time solve is called,
/// this may be updated in the future, but for now the goal is ensuring correctness rather
/// than strictly performance (though the model generation should be fairly fast, and
/// the usage of sparse matrices reduces the memory impact). Additionally, since the
/// objective uses sparse matrices, and can't be updated with a different sparsity pattern,
/// the ability to update the objective is limited regardless.
#[derive(Clone, Debug)]
pub struct ClarabelSolver {
    /// Whether the problem is to maximize or minimize the objective expression
    /*
    NOTE: Clarabel only solves minimization problems, so for maximization problems the coefficients
    in the objective will have to be multiplied by -1. They will be stored with the same sign
    passed in until the actual Clarabel problem is being constructed (since the objective sense
    could be updated anytime before then).
    */
    objective_sense: ObjectiveSense,
    /// Variables in the problem, maps variable id to the variable index
    variables: IndexMap<String, usize>,
    /// Number of variables currently in problem
    num_variables: usize,
    /// Matrix representing the quadratic terms in the objective, converted to a Csc matrix
    /// to be passed to clarabel
    ///
    /// #### Notes
    /// - Clarabel only solves minimization problems, so for maximization problems all the
    ///   coefficients in the objective are multiplied by -1. The coefficients stored in this
    ///   matrix match the coefficients of the variables as they are passed in, and they
    ///   are multiplied by -1 right before the problem is passed into Clarabel (to allow for the
    ///   objective sense to be changed without causing issues)
    /// - The COO matrix can hold multiple values for a single entry (i.e. it can have multiple
    ///   values stored in the (1,3) entry), these will be summed during conversion
    ///   to a CSC when passing the problem to Clarabel
    quadratic_objective: CooMatrix<f64>,
    /// Vec representing the linear terms in the objective
    ///
    /// #### Note
    ///  - Clarabel only solves minimization problems, so for maximization problems all the
    ///    coefficients in the objective are multiplied by -1. The coefficients stored in this
    ///    vector match the coefficients of the variables as they are passed in, and they
    ///    are multiplied by -1 right before the problem is passed into Clarabel (to allow for the
    ///    objective sense to be changed without causing issues)
    linear_objective: Vec<f64>,
    /// Constraints where some linear combination of variables equals a value
    equality_constraints: Vec<ClarabelEqualityConstraint>,
    /// Constraints where a linear combination of variables is less than a value
    inequality_constraints: Vec<ClarabelInequalityConstraint>,
    /// Number of equality constraints currently in the model
    num_equality_constraints: usize,
    /// Number of inequality constraints currently in the model
    num_inequality_constraints: usize,
}

impl Solver for ClarabelSolver {
    fn quadratic_objective_capable(&self) -> bool {
        true
    }

    fn integer_variable_capable(&self) -> bool {
        false
    }

    fn binary_variable_capable(&self) -> bool {
        false
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
        Err(SolverError::BadVariable {
            variable_id: id.to_string(),
            message:
                "Clarabel solver can't handle binary variables, try using SCIP or OSQP solvers"
                    .to_string(),
        })
    }

    fn add_integer_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        Err(SolverError::BadVariable {
            variable_id: id.to_string(),
            message:
                "Clarabel solver can't handle integer variables, try using SCIP or OSQP solvers"
                    .to_string(),
        })
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

/// Additional methods for working with the solver
impl ClarabelSolver {}

/// Constraints where the lhs is a linear combination of variables
#[derive(Clone, Debug)]
struct ClarabelEqualityConstraint {
    /// Variables in the constraint, order must match coefficients
    variables: Vec<String>,
    /// Coefficients of the variables, order must match variables
    coefficients: Vec<f64>,
    /// The right hand side of the constraint, sum(variable_i*coefficient_i) = `equals`
    equals: f64,
}

/// Struct representing inequality constraints for Clarabel
///
/// #### Note
/// - Clarabel can only really represent less than equal constraints (with the
///   non-negative orthant cone), so any greater than constraints will have to be converted
///   (by multiplying coefficients by -1), and the Problem constraints will have to be split
///   into 2 constraints, one representing each of the upper and lower bounds
#[derive(Clone, Debug)]
struct ClarabelInequalityConstraint {
    variables: Vec<String>,
    coefficients: Vec<f64>,
    less_than: f64,
}
