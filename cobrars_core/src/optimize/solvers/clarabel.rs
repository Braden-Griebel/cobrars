//! Implements a solver interface for Clarabel
use crate::optimize::objective::ObjectiveSense;
use crate::optimize::solvers::{Solver, SolverError};
use crate::optimize::{OptimizationStatus, ProblemSolution};
use clarabel;
use clarabel::algebra;
use clarabel::algebra::BlockConcatenate;
use clarabel::solver::{IPSolver, SolverStatus};
use indexmap::IndexMap;
use nalgebra_sparse::{coo, csc};

/// Solver backend to interface with Clarabel
///
/// #### Notes
/// - Currently, this interface essentially recreates the problem each time solve is called,
///   this may be updated in the future, but for now the goal is ensuring correctness rather
///   than strictly performance (though the model generation should be fairly fast, and
///   the usage of sparse matrices reduces the memory impact). Additionally, since the
///   objective uses sparse matrices, and can't be updated with a different sparsity pattern,
///   the ability to update the objective is limited regardless.
/// - Clarabel formulates problems as minimization problems, and requires that the P matrix 
///   (representing the quadratic objective) is positive semi-definite. This can cause issues
///   when trying to maximize a problem with a quadratic objective component (since the 
///   maximization problem is formulated by multiplying P by -1, which can result in a 
///   matrix that is not positive semi-definite). Further, in general caution is required to 
///   ensure that the P matrix is indeed positive semi-definite, as if it is not this can 
///   lead to difficult to diagnose errors with the Clarabel solver.  
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
    ///   quadratic objective match the coefficients of the variables as they are passed in, and they
    ///   are multiplied by -1 right before the problem is passed into Clarabel (to allow for the
    ///   objective sense to be changed without causing issues)
    /// - The quadratic objective can hold multiple values for a single entry (i.e. it can have multiple
    ///   values stored in the (1,3) entry), these will be summed during conversion
    ///   to a CSC when passing the problem to Clarabel
    quadratic_objective: ClarabelQuadraticObjective,
    /// Vec representing the linear terms in the objective
    ///
    /// #### Note
    ///  - Clarabel only solves minimization problems, so for maximization problems all the
    ///    coefficients in the objective are multiplied by -1. The coefficients stored in this
    ///    IndexMap match the coefficients of the variables as they are passed in, and they
    ///    are multiplied by -1 right before the problem is passed into Clarabel (to allow for the
    ///    objective sense to be changed without causing issues)
    linear_objective: IndexMap<String, f64>,
    /// Constraints where some linear combination of variables equals a value
    equality_constraints: Vec<ClarabelEqualityConstraint>,
    /// Constraints where a linear combination of variables is less than a value
    inequality_constraints: Vec<ClarabelInequalityConstraint>,
    /// Number of equality constraints currently in the model
    num_equality_constraints: usize,
    /// Number of inequality constraints currently in the model
    num_inequality_constraints: usize,
    /// Solver settings for Clarabel, see
    /// [Clarabel Documentation](https://docs.rs/clarabel/latest/clarabel/index.html) for
    /// more information
    solver_settings: clarabel::solver::DefaultSettings<f64>,
}

impl Solver for ClarabelSolver {
    /// Clarabel supports quadratic objective functions
    ///
    /// # Returns
    /// true
    fn quadratic_objective_capable(&self) -> bool {
        true
    }

    /// Clarabel doesn't support integer variables
    ///
    /// # Returns
    /// false
    fn integer_variable_capable(&self) -> bool {
        false
    }

    /// Clarabel doesn't support binary variables
    ///
    /// # Returns
    /// false
    fn binary_variable_capable(&self) -> bool {
        false
    }

    /// Adds a continuous variable to the model
    fn add_continuous_variable(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        self.variables.insert(id.to_string(), self.num_variables);
        self.num_variables += 1;
        // Update the num_variables in the quadratic objective
        self.quadratic_objective.num_variables = self.num_variables;
        // Add the upper bound
        self.add_less_equal_constraint(
            format!("{}_upper_bound", id),
            vec![id],
            vec![1.],
            upper_bound,
        );
        // Add the lower bound
        self.add_greater_equal_constraint(
            format!("{}_lower_bound", id),
            vec![id],
            vec![1.],
            lower_bound,
        );

        Ok(())
    }

    /// Clarabel doesn't support binary variables, so this will return an error
    fn add_binary_variable(&mut self, id: &str) -> Result<(), SolverError> {
        Err(SolverError::BadVariable {
            variable_id: id.to_string(),
            message:
                "Clarabel solver can't handle binary variables, try using SCIP or OSQP solvers"
                    .to_string(),
        })
    }

    /// Clarabel doesn't support integer variables, so this will return an error
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

    /// Adds an equality constraint to the Clarabel model
    fn add_equality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        equals: f64,
    ) -> Result<(), SolverError> {
        self.equality_constraints.push(ClarabelEqualityConstraint {
            id: id.to_string(),
            variables: variables.iter().map(|id| id.to_string()).collect(),
            coefficients,
            equals,
        });
        Ok(())
    }

    /// Adds an inequality constraint to the Clarabel model
    fn add_inequality_constraint(
        &mut self,
        id: &str,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), SolverError> {
        // The constraint needs to be split into 2, the lower and upper bound
        self.add_less_equal_constraint(
            format!("{}_upper", id),
            variables.clone(),
            coefficients.clone(),
            upper_bound,
        );
        self.add_greater_equal_constraint(
            format!("{}_lower", id),
            variables.clone(),
            coefficients.clone(),
            lower_bound,
        );
        // NOTE: The constraint counts are updated within
        // the methods above, so no need to update them here
        Ok(())
    }

    fn add_linear_objective_term(
        &mut self,
        variable_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError> {
        let coef = self
            .linear_objective
            .entry(variable_id.to_string())
            .or_insert(0.);
        *coef += coefficient;
        Ok(())
    }

    fn add_quadratic_objective_term(
        &mut self,
        variable1_id: &str,
        variable2_id: &str,
        coefficient: f64,
    ) -> Result<(), SolverError> {
        // Get the indices of the variables
        let variable1_index = self.get_variable_index(variable1_id)?;
        let variable2_index = self.get_variable_index(variable2_id)?;

        // Add the term to the objective
        self.quadratic_objective.add_term(
            variable1_index.clone(),
            variable2_index.clone(),
            coefficient,
        );

        // Term successfully added
        Ok(())
    }

    fn set_objective_sense(&mut self, objective_sense: ObjectiveSense) -> Result<(), SolverError> {
        self.objective_sense = objective_sense;
        Ok(())
    }

    fn clear_objective(&mut self) -> Result<(), SolverError> {
        self.linear_objective = IndexMap::<String, f64>::default();
        self.quadratic_objective.clear();
        Ok(())
    }

    fn solve(&mut self) -> Result<ProblemSolution, SolverError> {
        // Determine the multiplier for objective coefficients to
        // correctly reflect the objective sense
        let objective_multiplier = match self.objective_sense {
            ObjectiveSense::Minimize => 1.,
            ObjectiveSense::Maximize => -1.,
        };

        // Create the quadratic part of the objective
        // Start by creating a nalgebra COO matrix
        let quad_obj_mat_coo = self.quadratic_objective.as_coo(); // P in Clarabel
                                                                  // Convert the matrix to a CSC
        let quad_obj_mat_csc = csc::CscMatrix::from(&quad_obj_mat_coo) * objective_multiplier;

        // Disassemble to get the col offsets, row indices and values
        let (col_offsets, row_indices, values) = quad_obj_mat_csc.disassemble();
        
        // Create the Clarabel P matrix
        let p = algebra::CscMatrix::new(
            self.num_variables,
            self.num_variables,
            col_offsets,
            row_indices,
            values,
        );

        // Create the linear part of the objective
        let mut linear_objective: Vec<f64> = vec![0.; self.num_variables];
        for (var_id, var_coef) in self.linear_objective.iter() {
            let var_index = self.get_variable_index(var_id)?;
            linear_objective[var_index] += var_coef * objective_multiplier; // Explicitly want to allow repeats to be summed
        }

        // Create the equality constraint matrix (A_eq in Clarabel)
        // Create a vector to hold the RHS of the constraints,
        let mut equality_rhs = vec![0.; self.num_equality_constraints];

        // Start with an empty matrix
        let mut equality_cons_mat_coo: coo::CooMatrix<f64> =
            coo::CooMatrix::new(self.num_equality_constraints, self.num_variables);
        for (constraint, cons_index) in self
            .equality_constraints
            .iter()
            .zip(0..self.num_equality_constraints)
        {
            // Update the rhs
            equality_rhs[cons_index] = constraint.equals;
            // Now update the variables
            for (var, coef) in constraint
                .variables
                .iter()
                .zip(constraint.coefficients.iter())
            {
                let var_index = self.get_variable_index(var)?;
                equality_cons_mat_coo.push(var_index, cons_index, coef.clone());
            }
        }
        // Convert the matrix to a CSC
        let equality_cons_mat_csc = csc::CscMatrix::from(&equality_cons_mat_coo);
        let (col_offsets, row_indices, values) = equality_cons_mat_csc.disassemble();
        let a_eq = algebra::CscMatrix::new(
            self.num_equality_constraints,
            self.num_variables,
            col_offsets,
            row_indices,
            values,
        );

        // Create the inequality constraint matrix (A_ineq in Clarabel)
        // create a vector to hold the rhs of the constraints
        let mut inequality_rhs = vec![0.; self.num_inequality_constraints];

        // Start with an empty matrix
        let mut inequality_cons_mat_coo: coo::CooMatrix<f64> =
            coo::CooMatrix::new(self.num_inequality_constraints, self.num_variables);
        for (constraint, cons_index) in self
            .inequality_constraints
            .iter()
            .zip(0..self.num_inequality_constraints)
        {
            // update the rhs
            inequality_rhs[cons_index] = constraint.less_than;
            // Update the variables in A
            for (var, coef) in constraint
                .variables
                .iter()
                .zip(constraint.coefficients.iter())
            {
                let var_index = self.get_variable_index(var)?;
                inequality_cons_mat_coo.push(cons_index, var_index, coef.clone());
            }
        }
        // Convert the matrix to a CSC
        let inequality_cons_mat_csc = csc::CscMatrix::from(&inequality_cons_mat_coo);
        let (col_offsets, row_indices, values) = inequality_cons_mat_csc.disassemble();
        let a_ineq = algebra::CscMatrix::new(
            self.num_inequality_constraints,
            self.num_variables,
            col_offsets,
            row_indices,
            values,
        );

        // Combine the equality and inequality constraint matrices
        let a = algebra::CscMatrix::vcat(&a_eq, &a_ineq);
        // Combine the equality and inequality rhs
        equality_rhs.append(&mut inequality_rhs);
        let mut b = equality_rhs;

        // Create the cone specifications for the constraints
        let cones: Vec<clarabel::solver::SupportedConeT<f64>> = vec![
            clarabel::solver::ZeroConeT(self.num_equality_constraints),
            clarabel::solver::NonnegativeConeT(self.num_inequality_constraints),
        ];

        // Create the solver object
        assert!(p.check_format().is_ok());
        assert!(a.check_format().is_ok());
        let mut clarabel_solver = clarabel::solver::DefaultSolver::new(
            &p,
            &linear_objective,
            &a,
            &b,
            &cones,
            self.solver_settings.clone(),
        );

        // Solve the problem
        clarabel_solver.solve();

        // Now unpack the clarabel_solver to get the status, the variable values, and the dual values
        let opt_status = match clarabel_solver.solution.status {
            SolverStatus::Unsolved => OptimizationStatus::Unoptimized,
            SolverStatus::Solved => OptimizationStatus::Optimal,
            SolverStatus::PrimalInfeasible | SolverStatus::DualInfeasible => {
                OptimizationStatus::Infeasible
            }
            SolverStatus::AlmostSolved => OptimizationStatus::AlmostOptimal,
            SolverStatus::AlmostPrimalInfeasible | SolverStatus::AlmostDualInfeasible => {
                OptimizationStatus::Infeasible
            }
            SolverStatus::MaxIterations
            | SolverStatus::MaxTime
            | SolverStatus::InsufficientProgress => OptimizationStatus::SolverHalted,
            SolverStatus::NumericalError => OptimizationStatus::NumericalError,
        };

        // Determine the variable values
        let primal_values = clarabel_solver.solution.x;
        let mut variable_values: IndexMap<String, f64> = IndexMap::new();
        for (var_id, var_index) in self.variables.iter() {
            variable_values.insert(var_id.to_string(), primal_values[var_index.clone()].clone());
        }

        // Determine the objective value
        let objective_value = clarabel_solver.solution.obj_val;

        // Determine the shadow prices (dual variable values)
        let solver_dual_values = clarabel_solver.solution.z;
        let mut dual_values: IndexMap<String, f64> = IndexMap::new();
        for (cons_index, cons) in self.equality_constraints.iter().enumerate() {
            dual_values.insert(cons.id.to_string(), solver_dual_values[cons_index].clone());
        }
        for (cons, cons_index) in self.inequality_constraints.iter().zip(
            self.num_equality_constraints
                ..(self.num_equality_constraints + self.num_inequality_constraints),
        ) {
            dual_values.insert(cons.id.to_string(), solver_dual_values[cons_index].clone());
        }

        // Return the solution
        Ok(ProblemSolution {
            status: opt_status,
            objective_value: Some(objective_value * objective_multiplier),
            variable_values: Some(variable_values),
            dual_values: Some(dual_values),
        })
    }
}

/// Additional methods for working with the solver
impl ClarabelSolver {
    /// Create a new solver instance
    pub fn new() -> Self {
        let solver_settings = clarabel::solver::DefaultSettingsBuilder::default()
            .build()
            .unwrap();
        Self {
            objective_sense: ObjectiveSense::Minimize,
            variables: IndexMap::new(),
            num_variables: 0,
            quadratic_objective: ClarabelQuadraticObjective {
                rows: Vec::new(),
                cols: Vec::new(),
                values: Vec::new(),
                num_variables: 0,
            },
            linear_objective: IndexMap::new(),
            equality_constraints: Vec::new(),
            inequality_constraints: Vec::new(),
            num_equality_constraints: 0,
            num_inequality_constraints: 0,
            solver_settings,
        }
    }

    /// Set the Clarabel settings
    pub fn set_clarabel_settings(
        &mut self,
        clarabel_settings: clarabel::solver::DefaultSettings<f64>,
    ) {
        self.solver_settings = clarabel_settings;
    }

    /// Add a <= constraint to the model
    fn add_less_equal_constraint(
        &mut self,
        id: String,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        less_than: f64,
    ) {
        // Update inequality constraint count
        self.num_inequality_constraints += 1;
        // Add the inequality constraint to the model
        self.inequality_constraints
            .push(ClarabelInequalityConstraint {
                id,
                variables: variables.iter().map(|id| id.to_string()).collect(),
                coefficients,
                less_than,
            })
    }

    /// Add a >= constraint to the model
    fn add_greater_equal_constraint(
        &mut self,
        id: String,
        variables: Vec<&str>,
        coefficients: Vec<f64>,
        greater_than: f64,
    ) {
        // Multiply by negative 1, then use the add_less_equal constraint since that is equivalent
        self.add_less_equal_constraint(
            id,
            variables,
            coefficients.iter().map(|x| x * -1.).collect(),
            (greater_than * -1.),
        )
    }

    /// Get a variable index or return an error
    fn get_variable_index(&self, variable_id: &str) -> Result<usize, SolverError> {
        match self.variables.get(variable_id) {
            Some(index) => Ok(index.clone()),
            None => Err(SolverError::BadVariable {
                variable_id: variable_id.to_string(),
                message: "Variable not found in solver".to_string(),
            }),
        }
    }
}

/// Constraints where the lhs is a linear combination of variables
#[derive(Clone, Debug)]
struct ClarabelEqualityConstraint {
    /// id of the equality constraint
    id: String,
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
    /// id of the inequality constraint
    id: String,
    /// Variables in the constraint
    variables: Vec<String>,
    /// Coefficients of the variables
    coefficients: Vec<f64>,
    /// Right hand side of the constraint, translated to B in Clarabel
    less_than: f64,
}

/// Struct for holding the values in the quadratic objective, which
/// can be easily converted to a COO matrix (which can then be converted to a CSC
/// matrix for Clarabel)
#[derive(Clone, Debug)]
struct ClarabelQuadraticObjective {
    rows: Vec<usize>,
    cols: Vec<usize>,
    values: Vec<f64>,
    num_variables: usize,
}

impl ClarabelQuadraticObjective {
    /// Clear the quadratic objective
    fn clear(&mut self) {
        self.rows = Vec::new();
        self.cols = Vec::new();
        self.values = Vec::new();
    }

    /// Convert the quadratic objective into a COO
    fn as_coo(&self) -> coo::CooMatrix<f64> {
        coo::CooMatrix::try_from_triplets(
            self.num_variables,
            self.num_variables,
            self.rows.clone(),
            self.cols.clone(),
            self.values.clone(),
        )
        .unwrap()
    }

    /// Add a new term to the quadratic objective
    fn add_term(&mut self, var1_index: usize, var2_index: usize, coefficient: f64) {
        // Enforce the symmetry of the matrix
        /* Notes
            - The COO format allows duplicates, which will be summed when converted
            - The objective function is multiplied by 1/2, so the fact that entries along the
              diagonal are added twice, along with the allowed duplicates allows for the
              final values to be correct
        */
        self.rows.push(var1_index);
        self.cols.push(var2_index);
        self.values.push(coefficient);
        self.rows.push(var2_index);
        self.cols.push(var1_index);
        self.values.push(coefficient);
    }

    /// Add a variable to the objective (just updates the variable count)
    fn add_variable(&mut self) {
        self.num_variables += 1;
    }
}

#[cfg(test)]
mod tests {
    use clarabel::algebra::MatrixMathMut;
    use super::*;
    use clarabel::solver::NonnegativeConeT;
    #[test]
    fn linear_maximization() {
        let mut solver = ClarabelSolver::new();
        // Change the solver settings to stop the verbose output
        let solver_settings: clarabel::solver::DefaultSettings<f64> =
            clarabel::solver::DefaultSettingsBuilder::default()
                .verbose(false)
                .build()
                .unwrap();
        solver.set_clarabel_settings(solver_settings);
        // Add some positive variables
        solver.add_continuous_variable("x1", 0., 5.).unwrap();
        solver.add_continuous_variable("x2", 2., 6.).unwrap();
        // Set the objective to be 2*x1+3*x2, which is maximized at 28
        solver
            .set_objective_sense(ObjectiveSense::Maximize)
            .unwrap();
        solver.add_linear_objective_term("x1", 2.).unwrap();
        solver.add_linear_objective_term("x2", 3.).unwrap();

        // Solve the optimization problem
        let solution = solver.solve().unwrap();
        // Check the status
        assert_eq!(solution.status, OptimizationStatus::Optimal);
        // Check that the objective value is correct
        assert!((solution.objective_value.unwrap() - 28.).abs() < 1e-7);
        // Check the values of the variables
        let var_values = solution.variable_values.unwrap();
        assert!((var_values.get("x1").unwrap() - 5.).abs() < 1e-7);
        assert!((var_values.get("x2").unwrap() - 6.).abs() < 1e-7);
        // Check the values of the dual
        let dual_values = solution.dual_values.unwrap();
        assert!((dual_values.get("x1_upper_bound").unwrap() - 2.).abs() < 1e-7);
        assert!((dual_values.get("x2_upper_bound").unwrap() - 3.).abs() < 1e-7);
        assert!((dual_values.get("x1_lower_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x2_lower_bound").unwrap() - 0.).abs() < 1e-7);
    }

    #[test]
    fn linear_minimization() {
        let mut solver = ClarabelSolver::new();
        // Change the solver settings to stop the verbose output
        let solver_settings: clarabel::solver::DefaultSettings<f64> =
            clarabel::solver::DefaultSettingsBuilder::default()
                .verbose(false)
                .build()
                .unwrap();
        solver.set_clarabel_settings(solver_settings);
        // Add some positive variables
        solver.add_continuous_variable("x1", 0., 5.).unwrap();
        solver.add_continuous_variable("x2", 2., 6.).unwrap();
        // Set the objective to be 2*x1+3*x2, which is maximized at 28
        solver
            .set_objective_sense(ObjectiveSense::Minimize)
            .unwrap();
        solver.add_linear_objective_term("x1", 2.).unwrap();
        solver.add_linear_objective_term("x2", 3.).unwrap();

        // Solve the optimization problem
        let solution = solver.solve().unwrap();
        // Check the status
        assert_eq!(solution.status, OptimizationStatus::Optimal);
        // Check that the objective value is correct
        assert!((solution.objective_value.unwrap() - 6.).abs() < 1e-7);
        // Check the values of the variables
        let var_values = solution.variable_values.unwrap();
        assert!((var_values.get("x1").unwrap() - 0.).abs() < 1e-7);
        assert!((var_values.get("x2").unwrap() - 2.).abs() < 1e-7);
        // Check the values of the dual
        let dual_values = solution.dual_values.unwrap();
        assert!((dual_values.get("x1_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x2_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x1_lower_bound").unwrap() - 2.).abs() < 1e-7);
        assert!((dual_values.get("x2_lower_bound").unwrap() - 3.).abs() < 1e-7);
    }

    #[test]
    fn single_quadratic_minimization() {
        let mut solver = ClarabelSolver::new();
        // Change the solver settings to stop the verbose output
        let solver_settings: clarabel::solver::DefaultSettings<f64> =
            clarabel::solver::DefaultSettingsBuilder::default()
                .verbose(false)
                .build()
                .unwrap();
        solver.set_clarabel_settings(solver_settings);
        // Add some positive variables
        solver.add_continuous_variable("x1", 1., 8.).unwrap();
        solver.add_continuous_variable("x2", 3., 7.).unwrap();
        // Add some quadratic terms to the objective
        solver
            .set_objective_sense(ObjectiveSense::Minimize)
            .unwrap();
        solver.add_quadratic_objective_term("x1", "x2", 2.).unwrap();

        // Solve the optimization problem
        let solution = solver.solve().unwrap();
        // Check the status
        assert_eq!(solution.status, OptimizationStatus::Optimal);
        // Check that the objective value is correct
        assert!((solution.objective_value.unwrap() - 6.).abs() < 1e-7);
        // Check the values of the variables
        let var_values = solution.variable_values.unwrap();
        assert!((var_values.get("x1").unwrap() - 1.).abs() < 1e-7);
        assert!((var_values.get("x2").unwrap() - 3.).abs() < 1e-7);
        // Check the values of the dual
        let dual_values = solution.dual_values.unwrap();
        assert!((dual_values.get("x1_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x2_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x1_lower_bound").unwrap() - 6.).abs() < 1e-7);
        assert!((dual_values.get("x2_lower_bound").unwrap() - 2.).abs() < 1e-7);
    }

    #[test]
    fn multiple_quadratic_minimization() {
        let mut solver = ClarabelSolver::new();
        // Change the solver settings to stop the verbose output
        let solver_settings: clarabel::solver::DefaultSettings<f64> =
            clarabel::solver::DefaultSettingsBuilder::default()
                .verbose(false)
                .build()
                .unwrap();
        solver.set_clarabel_settings(solver_settings);
        // Add some positive variables
        solver.add_continuous_variable("x1", 5., 10.).unwrap();
        solver.add_continuous_variable("x2", 4., 12.).unwrap();
        // Add some quadratic terms to the objective creating x1*x1 + 2*x1*x2 + 3*x2*x2
        solver
            .set_objective_sense(ObjectiveSense::Minimize)
            .unwrap();
        solver.add_quadratic_objective_term("x1", "x2", 2.).unwrap();
        solver.add_quadratic_objective_term("x1", "x1", 1.).unwrap();
        solver.add_quadratic_objective_term("x2", "x2", 3.).unwrap();

        // Solve the optimization problem
        let solution = solver.solve().unwrap();
        // Check the status
        assert_eq!(solution.status, OptimizationStatus::Optimal);
        // Check that the objective value is correct
        assert!((solution.objective_value.unwrap() - (25. + 40. + 3. * 16.)).abs() < 1e-7);
        // Check the values of the variables
        let var_values = solution.variable_values.unwrap();
        assert!((var_values.get("x1").unwrap() - 5.).abs() < 1e-7);
        assert!((var_values.get("x2").unwrap() - 4.).abs() < 1e-7);
        // Check the values of the dual
        let dual_values = solution.dual_values.unwrap();
        assert!((dual_values.get("x1_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x2_upper_bound").unwrap() - 0.).abs() < 1e-7);
        assert!((dual_values.get("x1_lower_bound").unwrap() - 18.).abs() < 1e-7);
        assert!((dual_values.get("x2_lower_bound").unwrap() - 34.).abs() < 1e-7);
    }
}
