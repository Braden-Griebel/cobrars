//! Provides struct representing an optimization problem
use crate::optimize::constraint::Constraint;
use crate::optimize::objective::{Objective, ObjectiveSense, ObjectiveTerm};
use crate::optimize::problem::ProblemError::{
    NonExistentVariable, NonExistentVariablesInObjective,
};
use crate::optimize::solvers::Solver;
use crate::optimize::variable::{Variable, VariableBuilder, VariableType};
use indexmap::IndexMap;
use nalgebra::DimAdd;
use std::sync::{Arc, RwLock, RwLockReadGuard};
use thiserror::Error;

/// An optimization problem
#[derive(Debug, Clone)]
pub struct Problem {
    /// Objective to optimize
    objective: Objective,
    /// Variables of the optimization problem
    variables: IndexMap<String, Arc<RwLock<Variable>>>,
    /// Constraints of the optimization problem
    constraints: IndexMap<String, Arc<RwLock<Constraint>>>,
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
    // region Creation Functions
    /// Create a new optimization problem
    pub fn new(objective_sense: ObjectiveSense) -> Self {
        Self {
            objective: Objective::new(objective_sense),
            variables: IndexMap::new(),
            constraints: IndexMap::default(),
            status: OptimizationStatus::Unoptimized,
            variable_values: None,
            problem_type: ProblemType::LinearContinuous,
            num_variables: 0,
            num_constraints: 0,
        }
    }

    /// Create a new maximization problem
    pub fn new_maximization() -> Self {
        Self::new(ObjectiveSense::Maximize)
    }

    /// Create a new minimization problem
    pub fn new_minimization() -> Self {
        Self::new(ObjectiveSense::Minimize)
    }

    // endregion Creation Functions

    // region Update Objective Sense
    /// Update the objective sense of the problem
    pub fn update_objective_sense(&mut self, sense: ObjectiveSense) {
        self.objective.set_sense(sense);
    }
    // endregion Update Objective Sense

    // region Adding Variables
    /// Add a variable to the optimization problem
    pub fn add_variable(&mut self, variable: Arc<RwLock<Variable>>) -> Result<(), ProblemError> {
        // Validate that the variable can in fact be added to the problem
        self.validate_variable(variable.clone())?;
        // Update the index of the variable to reflect the current variable count
        variable.write().unwrap().index = self.num_variables;
        // Update the total number of variables
        self.num_variables += 1;
        // Insert the variable into the variables IndexMap
        let var_id = variable.read().unwrap().id.clone();
        self.variables.insert(var_id, variable.clone());
        // Update the type of the model if needed
        match variable.read().unwrap().variable_type {
            VariableType::Continuous => {
                // This will not change the type
            }
            VariableType::Integer|VariableType::Binary => {
                match self.problem_type {
                    ProblemType::LinearContinuous => {
                        self.problem_type = ProblemType::LinearMixedInteger;
                    }
                    ProblemType::QuadraticContinuous => {
                        self.problem_type = ProblemType::QuadraticMixedInteger;
                    }
                    _=>{}
                }
            }
        }
        Ok(())
    }

    /// Create a new variable and add it to the optimization problem
    pub fn add_new_variable(
        &mut self,
        id: &str,
        name: Option<&str>,
        variable_type: VariableType,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        let new_var = match name {
            Some(name) => VariableBuilder::default()
                .id(id)
                .name(name)
                .variable_type(variable_type)
                .lower_bound(lower_bound)
                .upper_bound(upper_bound)
                .build()
                .unwrap()
                .wrap(),
            None => VariableBuilder::default()
                .id(id)
                .variable_type(variable_type)
                .lower_bound(lower_bound)
                .upper_bound(upper_bound)
                .build()
                .unwrap()
                .wrap(),
        };
        self.add_variable(new_var.clone())
    }
    // endregion Adding Variables

    // region Adding Constraints
    /// Add a constraint to the problem
    pub fn add_constraint(
        &mut self,
        constraint: Arc<RwLock<Constraint>>,
    ) -> Result<(), ProblemError> {
        self.validate_constraint(constraint.clone())?;
        self.num_constraints += 1;
        self.constraints
            .insert(constraint.read().unwrap().get_id(), constraint.clone());
        Ok(())
    }

    /// Create a new equality constraint and add it to the model
    pub fn add_new_equality_constraint(
        &mut self,
        id: &str,
        variables: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
        equals: f64,
    ) -> Result<(), ProblemError> {
        let new_cons = Constraint::new_equality(id, variables, coefficients, equals).wrap();
        self.add_constraint(new_cons)
    }

    /// Create a new equality constraint using variable ids rather than variables references, and add it to the model
    pub fn add_new_equality_constraint_by_id(
        &mut self,
        id: &str,
        variables: &[&str],
        coefficients: &[f64],
        equals: f64,
    ) -> Result<(), ProblemError> {
        let variables = variables
            .iter()
            .map(|v_id| self.variables.get(*v_id).unwrap().clone())
            .collect::<Vec<_>>();
        self.add_new_equality_constraint(id, &variables, coefficients, equals)
    }

    /// Create a new inequality constraint and add it to the model
    pub fn add_new_inequality_constraint(
        &mut self,
        id: &str,
        variables: &[Arc<RwLock<Variable>>],
        coefficients: &[f64],
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        let new_cons =
            Constraint::new_inequality(id, variables, coefficients, lower_bound, upper_bound)
                .wrap();
        self.add_constraint(new_cons)
    }

    /// Create a new inequality constraint using variable ids rather than variable references, and add it to the model
    pub fn add_new_inequality_constraint_by_id(
        &mut self,
        id: &str,
        variables: &[&str],
        coefficients: &[f64],
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        let variables = variables
            .iter()
            .map(|v_id| self.variables.get(*v_id).unwrap().clone())
            .collect::<Vec<_>>();
        self.add_new_inequality_constraint(id, &variables, coefficients, lower_bound, upper_bound)
    }

    // endregion Adding Constraints

    // region Adding Objective Terms
    /// Add a new term to the objective
    pub fn add_objective_term(
        &mut self,
        objective_term: ObjectiveTerm,
    ) -> Result<(), ProblemError> {
        self.validate_objective_term(&objective_term)?;
        if let ObjectiveTerm::Quadratic { .. } = &objective_term {
            match self.problem_type {
                ProblemType::LinearContinuous => {
                    self.problem_type = ProblemType::QuadraticContinuous;
                }
                ProblemType::QuadraticContinuous => {
                    self.problem_type = ProblemType::QuadraticContinuous;
                }
                ProblemType::LinearMixedInteger => {
                    self.problem_type = ProblemType::QuadraticMixedInteger;
                }
                ProblemType::QuadraticMixedInteger => {
                    self.problem_type = ProblemType::QuadraticMixedInteger;
                }
            }
        }
        self.objective.add_term(objective_term);
        Ok(())
    }

    /// Add a new linear term to the objective
    pub fn add_new_linear_objective_term(
        &mut self,
        variable: Arc<RwLock<Variable>>,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let objective_term = ObjectiveTerm::new_linear(variable, coefficient);
        self.add_objective_term(objective_term)
    }

    /// Add a new linear term to the objective using the variable id
    pub fn add_new_linear_objective_term_by_id(
        &mut self,
        variable_id: &str,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let variable = match self.variables.get(variable_id) {
            Some(variable) => variable.clone(),
            None => return Err(NonExistentVariablesInObjective),
        };
        self.add_new_linear_objective_term(variable, coefficient)
    }

    /// Add a new quadratic term to the objective
    pub fn add_new_quadratic_objective_term(
        &mut self,
        variable1: Arc<RwLock<Variable>>,
        variable2: Arc<RwLock<Variable>>,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let objective_term = ObjectiveTerm::new_quadratic(variable1, variable2, coefficient);
        self.add_objective_term(objective_term)
    }

    /// Add a new quadratic term to the objective using the variable ids
    pub fn add_new_quadratic_objective_term_by_id(
        &mut self,
        variable1: &str,
        variable2: &str,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let variable1 = match self.variables.get(variable1) {
            Some(variable) => variable.clone(),
            None => return Err(NonExistentVariablesInObjective),
        };
        let variable2 = match self.variables.get(variable2) {
            Some(variable) => variable.clone(),
            None => return Err(NonExistentVariablesInObjective),
        };
        self.add_new_quadratic_objective_term(variable1, variable2, coefficient)
    }

    // endregion Adding Objective Terms

    // region update variable bounds
    /// Update the bounds of a variable
    pub fn update_variable_bounds(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        if lower_bound > upper_bound {
            return Err(ProblemError::InvalidVariableBounds);
        }
        match self.variables.get(id) {
            Some(var) => {
                var.write().unwrap().lower_bound = lower_bound;
                var.write().unwrap().upper_bound = upper_bound;
            }
            None => return Err(NonExistentVariable),
        };
        Ok(())
    }

    // endregion update variable bounds

    // region Remove Variables
    /// Remove a variable from the problem, will also remove it as a term from all constraints
    /// and any terms in the objective that include this variable
    pub fn delete_variable(&mut self, variable_id: &str) -> Result<(), ProblemError> {
        // Start by removing any terms in the objective including this variable
        self.objective.remove_terms_with_variable(variable_id);
        // Now remove any terms from constraints which include the variable
        self.constraints.iter().for_each(|(_, cons)| {
            cons.write().unwrap().remove_variable(variable_id);
        });
        // Finally the variable can be dropped from the model
        match self.variables.get(variable_id) {
            Some(_) => {
                self.variables.shift_remove(variable_id);
            }
            None => return Err(NonExistentVariable),
        };
        // And fix the index of the variables, as well as the count
        self.fix_variable_indices_and_count();
        Ok(())
    }

    // endregion Remove Variables

    // region Remove Constraints

    /// Remove a constraint (by id) from the model
    pub fn remove_constraint(&mut self, constraint_id: &str) {
        // Should be able to just drop the constraint
        self.constraints.shift_remove(constraint_id);
        // Update constraint count
        self.fix_constraint_count();
    }
    // endregion Remove Constraints

    // region Remove Objective Terms
    /// Remove all terms from the objective
    pub fn remove_all_objective_terms(&mut self) {
        self.objective.remove_all_terms();
        // Demote problem type if needed
        match self.problem_type {
            ProblemType::LinearContinuous => self.problem_type = ProblemType::LinearContinuous,
            ProblemType::QuadraticContinuous => self.problem_type = ProblemType::LinearContinuous,
            ProblemType::LinearMixedInteger => self.problem_type = ProblemType::LinearMixedInteger,
            ProblemType::QuadraticMixedInteger => {
                self.problem_type = ProblemType::QuadraticMixedInteger
            }
        }
    }
    // endregion Remove Objective Terms

    // region Validation Functions
    /// Check that a variable to be added is valid to add to this problem
    fn validate_variable(&self, variable: Arc<RwLock<Variable>>) -> Result<(), ProblemError> {
        // Check if there is already a variable with this id
        if self.variables.get(&variable.read().unwrap().id).is_some() {
            return Err(ProblemError::VariableIdAlreadyExists);
        };
        // Check if the variable bounds are valid
        let lb = variable.read().unwrap().lower_bound;
        let ub = variable.read().unwrap().upper_bound;
        if lb > ub {
            return Err(ProblemError::InvalidVariableBounds);
        }
        Ok(())
    }

    /// Check that a constraint to be added is valid to add to this Problem
    fn validate_constraint(&self, constraint: Arc<RwLock<Constraint>>) -> Result<(), ProblemError> {
        // Check that a variable with the same id doesn't already exist
        if self
            .constraints
            .get(&constraint.read().unwrap().get_id())
            .is_some()
        {
            return Err(ProblemError::ConstraintAlreadyExists);
        }
        // Check that for inequality constraints the bounds make sense
        match *constraint.read().unwrap() {
            Constraint::Equality { .. } => {}
            Constraint::Inequality {
                lower_bound,
                upper_bound,
                ..
            } => {
                if lower_bound > upper_bound {
                    return Err(ProblemError::InvalidConstraintBounds);
                }
            }
        }
        // Check that the variables in this constraint are in the model
        for var in constraint.read().unwrap().get_variables() {
            if let Some(problem_var) = self.variables.get(&var.read().unwrap().id) {
                if !Arc::ptr_eq(&var, problem_var) {
                    return Err(ProblemError::NonExistentVariablesInConstraint);
                }
            } else {
                return Err(ProblemError::NonExistentVariablesInConstraint);
            }
        }
        // All checks have passed
        Ok(())
    }

    /// Check that an objective term to be added is valid to add to this Problem
    fn validate_objective_term(&self, objective_term: &ObjectiveTerm) -> Result<(), ProblemError> {
        // make sure the variables in the objective are in the model
        match objective_term {
            ObjectiveTerm::Quadratic { var1, var2, .. } => {
                if let Some(problem_var1) = self.variables.get(&var1.read().unwrap().id) {
                    if !Arc::ptr_eq(var1, problem_var1) {
                        return Err(ProblemError::NonExistentVariablesInObjective);
                    }
                } else {
                    return Err(ProblemError::NonExistentVariablesInObjective);
                }
            }
            ObjectiveTerm::Linear { var, .. } => {
                if let Some(problem_var) = self.variables.get(&var.read().unwrap().id) {
                    if !Arc::ptr_eq(var, problem_var) {
                        return Err(ProblemError::NonExistentVariablesInObjective);
                    }
                } else {
                    return Err(ProblemError::NonExistentVariablesInObjective);
                }
            }
        }

        Ok(())
    }

    // endregion Validation Functions

    // region Fix Problem Functions
    /*
    Functions to fix the problem in various ways, such as making sure the variable indices are
    correct
    */
    fn fix_variable_indices_and_count(&mut self) {
        let num_variables = self.variables.len();
        self.variables
            .iter()
            .zip(0..num_variables)
            .for_each(|((_, var), ind)| {
                var.write().unwrap().index = ind;
            });
        self.num_variables = num_variables;
    }

    fn fix_constraint_count(&mut self) {
        let num_constraints = self.constraints.len();
        self.num_constraints = num_constraints;
    }

    fn fix_problem_type(&mut self) {
        let integer_variables = self.has_integer_variables();
        let quadratic_objective = self.has_quadratic_objective_terms();
        if integer_variables && quadratic_objective {
            self.problem_type = ProblemType::QuadraticMixedInteger
        } else if !integer_variables && quadratic_objective {
            self.problem_type = ProblemType::QuadraticContinuous
        } else if integer_variables && !quadratic_objective {
            self.problem_type = ProblemType::LinearMixedInteger
        } else if !quadratic_objective && !integer_variables {
            self.problem_type = ProblemType::QuadraticContinuous
        } else {
            panic!("Something other than true or false returned as bool!")
        }
    }

    // endregion Fix Problem Functions

    // region Check Problem
    /*
    Functions for checking properties of the Problem, such as if integer variables are
    present, if the objective contains quadratic terms, etc.
    */
    pub fn has_integer_variables(&self) -> bool {
        for (_, var) in &self.variables {
            if var.read().unwrap().variable_type == VariableType::Integer {
                return true;
            }
        }
        false
    }

    pub fn has_quadratic_objective_terms(&self) -> bool {
        self.objective.contains_quadratic()
    }

    // endregion Check Problem
}

/// Struct representing the solution to an optimization problem
pub struct ProblemSolution {
    /// The status of the optimization problem, representing if the optimization was
    /// completed successfully
    status: OptimizationStatus,
    /// Optimized value of the objective
    ///
    /// Some(f64) if the optimization was completed successfully, None otherwise
    objective_value: Option<f64>,
    /// Values of the variables at the optimum,
    ///
    /// Some(IndexMap), keyed by variable id, with values corresponding to variable
    /// values at optimum if the problem could be solved, None otherwise
    variable_values: Option<IndexMap<Variable, f64>>,
    /// Values of the dual variables at the optimum
    ///
    /// Some(IndexMap), keyed by constraint id, with values corresponding to dual
    /// variable values at optimum if the problem could be solved, and the solver
    /// supports retrieving the dual values, None otherwise
    dual_values: Option<IndexMap<Variable, f64>>,
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
#[derive(Clone, Debug, PartialEq)]
pub enum ProblemType {
    /// Problem with linear objectives and constraints, and continuous variables
    LinearContinuous,
    /// Problem with quadratic objective, linear constraints, and continuous variables
    QuadraticContinuous,
    /// Problem with linear objective and constraints, with integer and continuous variables
    LinearMixedInteger,
    /// Problem with a quadratic objective function, and some integer variables
    ///
    /// # Note:
    /// This problem type is not currently supported by any of the solvers
    QuadraticMixedInteger,
}

/// Errors associated with the Problem
#[derive(Error, Debug, Clone)]
pub enum ProblemError {
    /// Error when trying to add a variable with the same id as an existing variable
    #[error("Tried to add a variable with the same id as an existing variable")]
    VariableIdAlreadyExists,
    /// Error when trying to add variable with invalid bounds
    #[error("Tried to add a variable with lower_bound>upper_bound")]
    InvalidVariableBounds,
    /// Error when trying to add a constraint with the same id as an existing constraint
    #[error("Tried to add a constraint with the same id as an existing constraint")]
    ConstraintAlreadyExists,
    /// Error when trying to add a constraint with invalid bounds
    #[error("Tried to add an inequality constraint with lower_bound > upper_bound")]
    InvalidConstraintBounds,
    /// Error when trying to add a constraint that contains variables not in the model
    #[error("Tried to add a constraint with variables not in the model")]
    NonExistentVariablesInConstraint,
    /// Error when trying to add an objective term which includes variables not in the model
    #[error("Tried adding an objective term with variables not in the model")]
    NonExistentVariablesInObjective,
    /// Error when trying to perform an update or drop on a variable that doesn't exist
    #[error("Tried to access a variable that doesn't exist")]
    NonExistentVariable,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn new_problem() {
        // Catch fire test
        let problem = Problem::new(ObjectiveSense::Maximize);

        // Check that the specific creation functions work
        let max_problem = Problem::new_maximization();
        assert_eq!(max_problem.objective.sense, ObjectiveSense::Maximize);

        let min_problem = Problem::new_minimization();
        assert_eq!(min_problem.objective.sense, ObjectiveSense::Minimize);
    }

    #[test]
    fn update_objective_sense() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem.update_objective_sense(ObjectiveSense::Minimize);
        assert_eq!(problem.objective.sense, ObjectiveSense::Minimize);
        problem.update_objective_sense(ObjectiveSense::Maximize);
        assert_eq!(problem.objective.sense, ObjectiveSense::Maximize);
    }

    #[test]
    fn add_variables() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);

        // Add a single variable
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        // Check that the variable is in fact added
        if let Some(var) = problem.variables.get("x") {
            assert_eq!(var.read().unwrap().variable_type, VariableType::Continuous);
            assert_eq!(var.read().unwrap().index, 0);
            assert!(
                (var.read().unwrap().lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect lower bound"
            );
            assert!(
                (var.read().unwrap().lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect upper bound"
            );
        } else {
            panic!("Variable not added to model")
        }
        // Check that the problem has the correct type
        assert_eq!(problem.problem_type, ProblemType::LinearContinuous);

        // Add another variable, this time an integer variable
        problem
            .add_new_variable("y", None, VariableType::Integer, 64., 100.)
            .unwrap();
        if let Some(var) = problem.variables.get("y") {
            assert_eq!(var.read().unwrap().variable_type, VariableType::Integer);
            assert_eq!(var.read().unwrap().index, 1);
            assert!(
                (var.read().unwrap().lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect lower bound"
            );
            assert!(
                (var.read().unwrap().lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect upper bound"
            );
        } else {
            panic!("Variable not added to model")
        }
        // Check that the problem has updated its type
        assert_eq!(problem.problem_type, ProblemType::LinearMixedInteger);
    }

    #[test]
    fn add_bad_variable() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);

        // Add a variable with bad bounds
        let res = problem.add_new_variable("x", None, VariableType::Continuous, 100., 64.);
        if let Err(ProblemError::InvalidVariableBounds) = res {
            // Intentionally blank
        } else {
            panic!("Invalid variable bounds not caught")
        }
    }

    #[test]
    fn add_constraint() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);

        // Add some variables
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        // Add an equality constraint
        problem.add_new_equality_constraint_by_id("test_constraint", &["x", "y"], &[2., 3.], 200.).unwrap();

        // Check that the constraint was correctly added
        let cons = problem.constraints.get("test_constraint").unwrap();
        match *(cons.clone().read().unwrap()) {
            Constraint::Equality {equals, ..} => {
                assert!((equals-200.).abs()<1e-25)
            }
            Constraint::Inequality {..} => panic!("Incorrect constraint type added")
        }
        
        // Add an inequality constraint
        problem.add_new_inequality_constraint_by_id("test_constraint", &["x", "y"], &[2., 3.], 100., 200.).unwrap();

        // Check that the constraint was correctly added
        let cons = problem.constraints.get("test_constraint").unwrap();
        match *(cons.clone().read().unwrap()) {
            Constraint::Inequality {lower_bound, upper_bound, ..} => {
                assert!((lower_bound-100.).abs()<1e-25);
                assert!((upper_bound-200.).abs()<1e-25);
            }
            Constraint::Equality {..} => panic!("Incorrect constraint type added")
        }
    }

    #[test]
    fn add_bad_constraint() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);

        // Add some variables
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        // Add an equality constraint
        if let Err(ProblemError::InvalidConstraintBounds) = problem
            .add_new_inequality_constraint_by_id(
                "bad_constraint",
                &["x", "y"],
                &[2., 3.],
                200.,
                100.,
            )
        {
        } else {
            panic!("Invalid constraint bounds not caught")
        }
    }
}
