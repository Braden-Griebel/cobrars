//! Provides struct representing an optimization problem
use indexmap::IndexMap;
use thiserror::Error;

use super::{OptimizationStatus, ProblemSolution};
use crate::optimize::constraint::Constraint;
use crate::optimize::objective::{Objective, ObjectiveSense, ObjectiveTerm};
use crate::optimize::problem::ProblemError::BadObjective;
use crate::optimize::solvers::{Solver, SolverError};
use crate::optimize::variable::{Variable, VariableBuilder, VariableType};

// region Problem Implementation
/// An optimization problem
#[derive(Debug, Clone)]
pub struct Problem {
    /// Objective to optimize
    objective: Objective,
    /// Variables of the optimization problem
    variables: IndexMap<String, Variable>,
    /// Constraints of the optimization problem
    constraints: IndexMap<String, Constraint>,
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

/// Methods for creating a Problem
impl Problem {
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
}

/// Methods for working with Problem variables
impl Problem {
    /// Add a variable to the optimization problem
    pub fn add_variable(&mut self, mut variable: Variable) -> Result<(), ProblemError> {
        // Validate that the variable can in fact be added to the problem
        self.validate_variable(&variable)?;
        // Update the index of the variable to reflect the current variable count
        variable.index = self.num_variables;
        // Update the total number of variables
        self.num_variables += 1;
        // Insert the variable into the variables IndexMap
        let var_id = variable.id.clone();
        self.variables.insert(var_id, variable.clone());
        // Update the type of the model if needed
        match variable.variable_type {
            VariableType::Continuous => {
                // This will not change the type
            }
            VariableType::Integer | VariableType::Binary => match self.problem_type {
                ProblemType::LinearContinuous => {
                    self.problem_type = ProblemType::LinearMixedInteger;
                }
                ProblemType::QuadraticContinuous => {
                    self.problem_type = ProblemType::QuadraticMixedInteger;
                }
                _ => {}
            },
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
                .unwrap(),
            None => VariableBuilder::default()
                .id(id)
                .variable_type(variable_type)
                .lower_bound(lower_bound)
                .upper_bound(upper_bound)
                .build()
                .unwrap(),
        };
        self.add_variable(new_var.clone())
    }

    /// Update the bounds of a variable
    pub fn update_variable_bounds(
        &mut self,
        id: &str,
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        if lower_bound > upper_bound {
            return Err(ProblemError::BadVariable {
                variable_id: id.to_string(),
                message: "Tried updating bounds with lower_bound > upper_bound".to_string(),
            });
        }
        match self.variables.get_mut(id) {
            Some(var) => {
                var.lower_bound = lower_bound;
                var.upper_bound = upper_bound;
            }
            None => {
                return Err(ProblemError::BadVariable {
                    variable_id: id.to_string(),
                    message: "Tried updating variable not in problem".to_string(),
                })
            }
        };
        Ok(())
    }

    /// Remove a variable from the problem, will also remove it as a term from all constraints
    /// and any terms in the objective that include this variable
    pub fn remove_variable(&mut self, variable_id: &str) -> Result<(), ProblemError> {
        // Start by removing any terms in the objective including this variable
        self.objective.remove_terms_with_variable(variable_id);
        // Now remove any terms from constraints which include the variable
        self.constraints.iter_mut().for_each(|(_, cons)| {
            cons.remove_variable(variable_id);
        });
        // Finally the variable can be dropped from the model
        match self.variables.get(variable_id) {
            Some(_) => {
                self.variables.shift_remove(variable_id);
            }
            None => {
                return Err(ProblemError::BadVariable {
                    variable_id: variable_id.to_string(),
                    message: "Tried removing variable which isn't in problem".to_string(),
                })
            }
        };
        // And fix the index of the variables, as well as the count
        self.fix_variable_indices_and_count();
        // fix the problem type
        self.fix_problem_type();
        Ok(())
    }

    /// Check that a variable to be added is valid to add to this problem
    fn validate_variable(&self, variable: &Variable) -> Result<(), ProblemError> {
        // Check if there is already a variable with this id
        if self.variables.get(&variable.id).is_some() {
            return Err(ProblemError::BadVariable {
                variable_id: variable.id.clone(),
                message: "Tried adding variable with same id as variable already in problem"
                    .to_string(),
            });
        };
        // Check if the variable bounds are valid
        let lb = variable.lower_bound;
        let ub = variable.upper_bound;
        if lb > ub {
            return Err(ProblemError::BadVariable {
                variable_id: variable.id.clone(),
                message: "Tried adding variable with lower_bound > upper_bound".to_string(),
            });
        }
        Ok(())
    }
}

/// Methods for working with Problem constraints
impl Problem {
    /// Add a constraint to the problem
    pub fn add_constraint(&mut self, constraint: Constraint) -> Result<(), ProblemError> {
        self.validate_constraint(&constraint)?;
        self.num_constraints += 1;
        self.constraints
            .insert(constraint.get_id(), constraint.clone());
        Ok(())
    }

    /// Create a new equality constraint and add it to the model
    pub fn add_new_equality_constraint(
        &mut self,
        id: &str,
        variables: &[&str],
        coefficients: &[f64],
        equals: f64,
    ) -> Result<(), ProblemError> {
        let new_cons = Constraint::new_equality(id, variables, coefficients, equals);
        self.add_constraint(new_cons)
    }

    /// Create a new inequality constraint and add it to the model
    pub fn add_new_inequality_constraint(
        &mut self,
        id: &str,
        variables: &[&str],
        coefficients: &[f64],
        lower_bound: f64,
        upper_bound: f64,
    ) -> Result<(), ProblemError> {
        let new_cons =
            Constraint::new_inequality(id, variables, coefficients, lower_bound, upper_bound);
        self.add_constraint(new_cons)
    }

    pub fn update_equality_constraint_bound(
        &mut self,
        constraint_id: &str,
        new_equals: f64,
    ) -> Result<(), ProblemError> {
        let cons = self
            .constraints
            .get_mut(constraint_id)
            .ok_or(ProblemError::BadConstraint {
                constraint_id: constraint_id.to_string(),
                message: "Tried updating constraint which isn't in Problem".to_string(),
            })?;
        match cons {
            Constraint::Equality { ref mut equals, .. } => {
                *equals = new_equals;
            }
            Constraint::Inequality { .. } => {
                return Err(ProblemError::BadConstraint {
                    constraint_id: constraint_id.to_string(),
                    message: "Tried updating inequality constraint with update_equality_constraint_bound method".to_string(),
                });
            }
        }
        Ok(())
    }

    pub fn update_inequality_constraint_bounds(
        &mut self,
        constraint_id: &str,
        new_lower_bound: f64,
        new_upper_bound: f64,
    ) -> Result<(), ProblemError> {
        if new_lower_bound > new_upper_bound {
            return Err(ProblemError::BadConstraint {
                constraint_id: constraint_id.to_string(),
                message: "Tried updating constraint bounds with lower_bound > upper_bound"
                    .to_string(),
            });
        }
        let cons = self
            .constraints
            .get_mut(constraint_id)
            .ok_or(ProblemError::BadConstraint {
                constraint_id: constraint_id.to_string(),
                message: "Tried updating constraint which is not in problem".to_string(),
            })?;
        match cons {
            Constraint::Equality { .. } => {
                return Err(ProblemError::BadConstraint {
                    constraint_id: constraint_id.to_string(),
                    message: "Tried updating equality constraint with update_inequality_constraint_bounds method".to_string(),
                });
            }
            Constraint::Inequality {
                ref mut lower_bound,
                ref mut upper_bound,
                ..
            } => {
                *lower_bound = new_lower_bound;
                *upper_bound = new_upper_bound;
            }
        };
        Ok(())
    }

    /// Remove a constraint (by id) from the model
    pub fn remove_constraint(&mut self, constraint_id: &str) {
        // Should be able to just drop the constraint
        self.constraints.shift_remove(constraint_id);
        // Update constraint count
        self.fix_constraint_count();
    }

    /// Check that a constraint to be added is valid to add to this Problem
    fn validate_constraint(&self, constraint: &Constraint) -> Result<(), ProblemError> {
        // Check that a variable with the same id doesn't already exist
        if self.constraints.get(&constraint.get_id()).is_some() {
            return Err(ProblemError::BadConstraint { constraint_id: constraint.get_id().to_string(), message: "Tried adding a constraint with the same id as a constraint already in the problem".to_string() });
        }
        // Check that for inequality constraints the bounds make sense
        match constraint {
            Constraint::Equality { .. } => {}
            Constraint::Inequality {
                lower_bound,
                upper_bound,
                ..
            } => {
                if lower_bound > upper_bound {
                    return Err(ProblemError::BadConstraint {
                        constraint_id: constraint.get_id().to_string(),
                        message: "Tried adding a constraint with lower_bound > upper_bound"
                            .to_string(),
                    });
                }
            }
        }
        // Check that the variables in this constraint are in the model
        for var in constraint.get_variables() {
            if let Some(problem_var) = self.variables.get(&var) {
                if var != problem_var.id {
                    return Err(ProblemError::BadConstraint { constraint_id: constraint.get_id().to_string(), message: "Tried adding a constraint which includes variables not found in problem".to_string() });
                }
            } else {
                return Err(ProblemError::BadConstraint {
                    constraint_id: constraint.get_id().to_string(),
                    message:
                        "Tried adding a constraint which includes variables not found in problem"
                            .to_string(),
                });
            }
        }
        // All checks have passed
        Ok(())
    }
}

/// Methods for working with Problem objective
impl Problem {
    /// Update the objective sense of the problem
    pub fn update_objective_sense(&mut self, sense: ObjectiveSense) {
        self.objective.set_sense(sense);
    }

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
        variable: &str,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let objective_term = ObjectiveTerm::new_linear(variable, coefficient);
        self.add_objective_term(objective_term)
    }

    /// Add a new quadratic term to the objective
    pub fn add_new_quadratic_objective_term(
        &mut self,
        variable1: &str,
        variable2: &str,
        coefficient: f64,
    ) -> Result<(), ProblemError> {
        let objective_term = ObjectiveTerm::new_quadratic(variable1, variable2, coefficient);
        self.add_objective_term(objective_term)
    }

    /// Remove all terms from the problem objective
    pub fn remove_all_objective_terms(&mut self) {
        self.objective.remove_all_terms();
        // Demote problem type if needed
        match self.problem_type {
            ProblemType::LinearContinuous | ProblemType::QuadraticContinuous => {
                self.problem_type = ProblemType::LinearContinuous
            }
            ProblemType::LinearMixedInteger | ProblemType::QuadraticMixedInteger => {
                self.problem_type = ProblemType::LinearMixedInteger
            }
        }
    }

    /// Check that an objective term to be added is valid to add to this Problem
    fn validate_objective_term(&self, objective_term: &ObjectiveTerm) -> Result<(), ProblemError> {
        // make sure the variables in the objective are in the model
        match objective_term {
            ObjectiveTerm::Quadratic { var1, var2, .. } => {
                // Check the first variable
                if let Some(problem_var1) = self.variables.get(var1) {
                    if *var1 != problem_var1.id {
                        return Err(BadObjective {message: format!("Objective term includes variable {var1}, which is not found in the problem")});
                    }
                } else {
                    return Err(BadObjective {message: format!("Objective term includes variable {var1}, which is not found in the problem")});
                }
                // Check the second variable
                if let Some(problem_var2) = self.variables.get(var2) {
                    if *var2 != problem_var2.id {
                        return Err(BadObjective {message: format!("Objective term includes variable {var2}, which is not found in the problem")});
                    }
                } else {
                    return Err(BadObjective {message: format!("Objective term includes variable {var2}, which is not found in the problem")});
                }
            }
            ObjectiveTerm::Linear { var, .. } => {
                if let Some(problem_var) = self.variables.get(var) {
                    if *var != problem_var.id {
                        return Err(BadObjective {message: format!("Objective term includes variable {var}, which is not found in the problem")});
                    }
                } else {
                    return Err(BadObjective {message: format!("Objective term includes variable {var}, which is not found in the problem")});
                }
            }
        }

        Ok(())
    }
}

/*
Methods for fixing model consistency, mainly to be used after deleting variables,
constraints, objectives etc. such that the variable counts/indices, problem type, and
constraint information remains consistent.
*/
/// Methods for fixing Problem consistency
impl Problem {
    fn fix_variable_indices_and_count(&mut self) {
        let num_variables = self.variables.len();
        self.variables
            .iter_mut()
            .zip(0..num_variables)
            .for_each(|((_, var), ind)| {
                var.index = ind;
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
}

/*
Functions for checking properties of the Problem, such as if integer variables are
present, if the objective contains quadratic terms, etc.
*/
/// Methods for checking Problem properties
impl Problem {
    /// Check whether the problem contains integer variables
    pub fn has_integer_variables(&self) -> bool {
        for (_, var) in &self.variables {
            if var.variable_type == VariableType::Integer {
                return true;
            }
        }
        false
    }

    /// Check whether the problem contains binary variables
    pub fn has_binary_variables(&self) -> bool {
        for (_, var) in &self.variables {
            if var.variable_type == VariableType::Binary {
                return true;
            }
        }
        false
    }

    /// Check whether the objective contains quadratic terms
    pub fn has_quadratic_objective_terms(&self) -> bool {
        self.objective.contains_quadratic()
    }
}

/// Methods for solving the optimization problem using one of the backend solvers
impl Problem {
    pub fn solve<T: Solver>(&mut self, solver: &mut T) -> Result<ProblemSolution, ProblemError> {
        self.construct_problem(solver)?;
        match solver.solve() {
            Ok(solution) => Ok(solution),
            Err(e) => Err(ProblemError::SolverError(e)),
        }
    }

    fn construct_problem<T: Solver>(&mut self, solver: &mut T) -> Result<(), ProblemError> {
        // Determine solver capabilities
        let handle_integer = solver.integer_variable_capable();
        let handle_quadratic = solver.quadratic_objective_capable();
        let handle_binary = solver.binary_variable_capable();
        // Make sure problem type is correct
        self.fix_problem_type();
        match self.problem_type {
            ProblemType::LinearContinuous => {
                // This must be able to be handled by all solvers
            }
            ProblemType::QuadraticContinuous => {
                if !handle_quadratic {
                    return Err(ProblemError::SolverProblemConflict(
                        "Solver can't handle quadratic objective terms".to_string(),
                    ));
                }
            }
            ProblemType::LinearMixedInteger => {
                if !handle_integer {
                    return Err(ProblemError::SolverProblemConflict(
                        "Solver can't handle integer variables".to_string(),
                    ));
                }
            }
            ProblemType::QuadraticMixedInteger => {
                if !handle_quadratic && handle_integer {
                    return Err(ProblemError::SolverProblemConflict(
                        "Solver can't handle quadratic objective terms".to_string(),
                    ));
                }
            }
        }

        if !handle_binary && self.has_binary_variables() {
            /* This should already be handled by the Integer Variable check, since
            if a solver can handle integer variables it can by definition handle binary variables.
            However, if one of the solvers throws an error on */
            return Err(ProblemError::SolverProblemConflict(
                "Solver can't handle binary variables".to_string(),
            ));
        }

        // Add variables to the solver
        for (id, var) in self.variables.iter() {
            match var.variable_type {
                VariableType::Continuous => {
                    solver.add_continuous_variable(id, var.lower_bound, var.upper_bound)?
                }
                VariableType::Integer => {
                    solver.add_integer_variable(id, var.lower_bound, var.upper_bound)?
                }
                VariableType::Binary => solver.add_binary_variable(id)?,
            }
        }
        // Add constraints to the solver
        for (id, cons) in self.constraints.iter() {
            match cons {
                Constraint::Equality { id, terms, equals } => {
                    let mut variables: Vec<&str> = Vec::new();
                    let mut coefficients: Vec<f64> = Vec::new();

                    for term in terms {
                        variables.push(&term.variable);
                        coefficients.push(term.coefficient);
                    }
                    solver.add_equality_constraint(id, variables, coefficients, equals.clone())?
                }
                Constraint::Inequality {
                    id,
                    terms,
                    lower_bound,
                    upper_bound,
                } => {
                    let mut variables: Vec<&str> = Vec::new();
                    let mut coefficients: Vec<f64> = Vec::new();

                    for term in terms {
                        variables.push(&term.variable);
                        coefficients.push(term.coefficient.clone())
                    }
                    solver.add_inequality_constraint(
                        id,
                        variables,
                        coefficients,
                        lower_bound.clone(),
                        upper_bound.clone(),
                    )?
                }
            }
        }

        // Add objective to the solver
        // Set the sense
        solver.set_objective_sense(self.objective.sense.clone());
        // Add all the terms
        for term in self.objective.terms.iter() {
            match term {
                ObjectiveTerm::Quadratic { var1, var2, coef } => {
                    solver.add_quadratic_objective_term(var1, var2, coef.clone())?
                }
                ObjectiveTerm::Linear { var, coef } => {
                    solver.add_linear_objective_term(var, coef.clone())?
                }
            }
        }
        Ok(())
    }
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
    QuadraticMixedInteger,
}

// endregion Problem Implementation

// region Error

/// Errors associated with the Problem
#[derive(Error, Debug, Clone)]
pub enum ProblemError {
    /// Error for bad variable access, addition, or update
    #[error("Invalid variable {variable_id}: {message}")]
    BadVariable {
        variable_id: String,
        message: String,
    },
    /// Error for bad constraint access, addition, or update
    #[error("Invalid constraint {constraint_id}: {message}")]
    BadConstraint {
        constraint_id: String,
        message: String,
    },
    /// Error for bad objective term access, addition, or update
    #[error("Invalid objective encountered: {message}")]
    BadObjective { message: String },
    /// Error when trying to use solver which can't handle the problem type
    #[error("Solver and problem have a conflict: {0}")]
    SolverProblemConflict(String),
    /// Error thrown from the solver
    #[error("Solver returned an error")]
    SolverError(#[from] SolverError),
}
// endregion Error

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
            assert_eq!(var.variable_type, VariableType::Continuous);
            assert_eq!(var.index, 0);
            assert!(
                (var.lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect lower bound"
            );
            assert!(
                (var.lower_bound - 64.0).abs() < 1e-25,
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
            assert_eq!(var.variable_type, VariableType::Integer);
            assert_eq!(var.index, 1);
            assert!(
                (var.lower_bound - 64.0).abs() < 1e-25,
                "Variable added with incorrect lower bound"
            );
            assert!(
                (var.lower_bound - 64.0).abs() < 1e-25,
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
        if let Err(ProblemError::BadVariable {
            variable_id,
            message,
        }) = res
        {
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
        problem
            .add_new_equality_constraint("test_equality_constraint", &["x", "y"], &[2., 3.], 200.)
            .unwrap();

        // Check that the constraint was correctly added
        let cons = problem.constraints.get("test_equality_constraint").unwrap();
        match cons.clone() {
            Constraint::Equality { equals, .. } => {
                assert!((equals - 200.).abs() < 1e-25)
            }
            Constraint::Inequality { .. } => panic!("Incorrect constraint type added"),
        }

        // Add an inequality constraint
        problem
            .add_new_inequality_constraint(
                "test_inequality_constraint",
                &["x", "y"],
                &[2., 3.],
                100.,
                200.,
            )
            .unwrap();

        // Check that the constraint was correctly added
        let cons = problem
            .constraints
            .get("test_inequality_constraint")
            .unwrap();
        match cons {
            Constraint::Inequality {
                lower_bound,
                upper_bound,
                ..
            } => {
                assert!((lower_bound - 100.).abs() < 1e-25);
                assert!((upper_bound - 200.).abs() < 1e-25);
            }
            Constraint::Equality { .. } => panic!("Incorrect constraint type added"),
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
        if let Err(ProblemError::BadConstraint { .. }) = problem.add_new_inequality_constraint(
            "bad_constraint",
            &["x", "y"],
            &[2., 3.],
            200.,
            100.,
        ) {
        } else {
            panic!("Invalid constraint bounds not caught")
        }
    }

    #[test]
    fn add_objective_term() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 25., 150.)
            .unwrap();

        // add a linear term to the objective
        problem.add_new_linear_objective_term("x", 26.).unwrap();
        // check that the problem is still a linear continuous problem
        assert_eq!(problem.problem_type, ProblemType::LinearContinuous);
        // Check that the objective is the right length
        assert_eq!(problem.objective.terms.len(), 1);
        // Check that the objective term matches the expected value
        let term = problem.objective.terms[0].clone();
        match problem.objective.terms[0] {
            ObjectiveTerm::Quadratic { .. } => {
                panic!("Incorrect objective term");
            }
            ObjectiveTerm::Linear { ref var, coef } => {
                let v = problem.variables.get(var).unwrap();
                if v.id != "x"
                    || v.variable_type != VariableType::Continuous
                    || (v.lower_bound - 64.).abs() > 1e-25
                    || (v.upper_bound - 100.).abs() > 1e-25
                    || (coef - 26.).abs() > 1e-25
                {
                    panic!("Incorrect Variable in Objective")
                }
            }
        }

        // Add a quadratic term to the objective
        problem
            .add_new_quadratic_objective_term("x", "y", 2.)
            .unwrap();
        // Check that the problem is now a quadratic continuous problem
        assert_eq!(problem.problem_type, ProblemType::QuadraticContinuous);
        assert_eq!(problem.objective.terms.len(), 2);

        match problem.objective.terms[1] {
            ObjectiveTerm::Quadratic {
                ref var1,
                ref var2,
                coef,
            } => {
                let v1 = problem.variables.get(var1).unwrap();
                if v1.id != "x"
                    || v1.variable_type != VariableType::Continuous
                    || (v1.lower_bound - 64.).abs() > 1e-25
                    || (v1.upper_bound - 100.).abs() > 1e-25
                {
                    panic!("Incorrect Variable in Objective")
                }

                let v2 = problem.variables.get(var2).unwrap();
                if v2.id != "y"
                    || v2.variable_type != VariableType::Continuous
                    || (v2.lower_bound - 25.).abs() > 1e-25
                    || (v2.upper_bound - 150.).abs() > 1e-25
                {
                    panic!("Incorrect Variable in Objective")
                }

                if (coef - 2.).abs() > 1e-25 {
                    panic!("Incorrect coefficient of quadratic objective term");
                }
            }
            ObjectiveTerm::Linear { .. } => {
                panic!("Incorrect objective term");
            }
        }

        // Now add an integer variable
        problem
            .add_new_variable("z", None, VariableType::Integer, 4., 10.)
            .unwrap();
        // Check that the problem type is now a QuadraticMixedInteger
        assert_eq!(problem.problem_type, ProblemType::QuadraticMixedInteger);
        assert_eq!(problem.objective.terms.len(), 2);
    }

    #[test]
    fn update_variable_bounds() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        if let Some(var) = problem.variables.get("x") {
            let v = var;
            if (v.lower_bound - 64.).abs() > 1e-25 {
                panic!("Incorrect variable bounds");
            }
            if (v.upper_bound - 100.).abs() > 1e-25 {
                panic!("Incorrect variable bounds");
            }
        } else {
            panic!("Variable not added correctly")
        }

        problem.update_variable_bounds("x", 4., 5.).unwrap();
        if let Some(var) = problem.variables.get("x") {
            let v = var;
            if (v.lower_bound - 4.).abs() > 1e-25 {
                panic!("Incorrect variable bounds");
            }
            if (v.upper_bound - 5.).abs() > 1e-25 {
                panic!("Incorrect variable bounds");
            }
        } else {
            panic!("Variable not added correctly")
        }
    }

    #[test]
    fn bad_update_variable_bounds() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        let res = problem.update_variable_bounds("x", 5., 4.);
        assert!(res.is_err());
        if let Err(ProblemError::BadVariable { .. }) = res {
        } else {
            panic!("Bad Variable Bounds Failed to be Caught")
        }
    }

    #[test]
    fn constraint_bounds_update() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        problem
            .add_new_equality_constraint("test_equality_constraint", &["x", "y"], &[2., 3.], 5.)
            .unwrap();

        problem
            .update_equality_constraint_bound("test_equality_constraint", 100.)
            .unwrap();

        if let Some(cons) = problem.constraints.get("test_equality_constraint") {
            match *(cons) {
                Constraint::Equality { equals, .. } => {
                    if (equals - 100.).abs() > 1e-25 {
                        panic!("Incorrect constraint bounds")
                    }
                }
                Constraint::Inequality { .. } => {
                    panic!("Incorrect constraint")
                }
            }
        }

        // Test inequality constraint update
        problem
            .add_new_inequality_constraint(
                "test_inequality_constraint",
                &["x", "y"],
                &[2., 3.],
                7.,
                10.,
            )
            .unwrap();

        problem
            .update_inequality_constraint_bounds("test_inequality_constraint", 150., 250.)
            .unwrap();

        if let Some(cons) = problem.constraints.get("test_inequality_constraint") {
            match *(cons) {
                Constraint::Equality { .. } => {
                    panic!("Incorrect constraint")
                }
                Constraint::Inequality {
                    lower_bound,
                    upper_bound,
                    ..
                } => {
                    if (lower_bound - 150.).abs() > 1e-25 {
                        panic!("Incorrect constraint bounds")
                    }
                    if (upper_bound - 250.).abs() > 1e-25 {
                        panic!("Incorrect constraint bounds")
                    }
                }
            }
        }
    }

    #[test]
    fn bad_update_constraint_bounds() {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        problem
            .add_new_inequality_constraint(
                "test_inequality_constraint",
                &["x", "y"],
                &[2., 3.],
                5.,
                6.,
            )
            .unwrap();
        // try to update an inequality with the update equality method
        let res = problem.update_equality_constraint_bound("test_inequality_constraint", 15.);
        println!("{:?}", res);
        if let Err(ProblemError::BadConstraint { .. }) = res {
            // This is expected
        } else {
            panic!("Missed bad bounds update");
        }

        problem
            .add_new_equality_constraint("test_equality_constraint", &["x", "y"], &[2., 3.], 15.)
            .unwrap();
        // Try to update an equality with the inequality constraint bounds
        let res =
            problem.update_inequality_constraint_bounds("test_equality_constraint", 250., 300.);
        if let Err(ProblemError::BadConstraint { .. }) = res {
            // This is expected
        } else {
            panic!("Missed bad bounds update");
        }

        // Try updating inequality bounds with bad bounds
        let res = problem.update_inequality_constraint_bounds("test_equality_constraint", 5., 4.);
        if let Err(ProblemError::BadConstraint { .. }) = res {
            // Expected
        } else {
            panic!("Missed bad bounds update");
        }
    }

    #[test]
    fn delete_variable() {
        let mut problem = get_test_problem();

        // Remove y
        problem.remove_variable("y").unwrap();
        // Check that the variable is no longer in variable
        assert!(problem.variables.get("y").is_none());

        // Check that the variable isn't in the constraints
        if let Some(cons) = problem.constraints.get("test_equality_constraint") {
            match *(cons) {
                Constraint::Equality { ref terms, .. } => {
                    assert_eq!(terms.len(), 1);
                    for t in terms {
                        if t.variable == "y" {
                            panic!("Variable not removed from constraint")
                        }
                    }
                }
                Constraint::Inequality { .. } => {
                    panic!("Incorrect constraint found");
                }
            }
        }

        if let Some(cons) = problem.constraints.get("test_inequality_constraint") {
            match *(cons) {
                Constraint::Inequality { ref terms, .. } => {
                    assert_eq!(terms.len(), 1);
                    for t in terms {
                        if t.variable == "y" {
                            panic!("Variable not removed from constraint")
                        }
                    }
                }
                Constraint::Equality { .. } => {
                    panic!("Incorrect constraint found");
                }
            }
        }

        // Check that the objective is empty
        assert_eq!(problem.objective.terms.len(), 0);
    }

    #[test]
    fn remove_constraint() {
        let mut problem = get_test_problem();

        assert_eq!(problem.constraints.len(), 2);

        problem.remove_constraint("test_equality_constraint");
        assert_eq!(problem.constraints.len(), 1);
        assert!(problem
            .constraints
            .get("test_equality_constraint")
            .is_none());
    }

    #[test]
    fn remove_all_objective_terms() {
        let mut problem = get_test_problem();
        assert_eq!(problem.objective.terms.len(), 1);
        problem.remove_all_objective_terms();
        assert_eq!(problem.objective.terms.len(), 0);
    }

    #[test]
    fn check_problem() {
        let mut problem = get_test_problem();
        assert!(!problem.has_integer_variables());
        assert!(!problem.has_quadratic_objective_terms());
        assert_eq!(problem.problem_type, ProblemType::LinearContinuous);

        problem
            .add_new_variable("z", None, VariableType::Integer, 64., 100.)
            .unwrap();
        assert!(problem.has_integer_variables());
        assert!(!problem.has_quadratic_objective_terms());
        assert_eq!(problem.problem_type, ProblemType::LinearMixedInteger);

        problem
            .add_new_quadratic_objective_term("x", "y", 5.)
            .unwrap();
        assert!(problem.has_integer_variables());
        assert!(problem.has_quadratic_objective_terms());
        assert_eq!(problem.problem_type, ProblemType::QuadraticMixedInteger);

        // Check that the removal of the z variable changes the type
        problem.remove_variable("z").unwrap();
        assert!(!problem.has_integer_variables());
        assert!(problem.has_quadratic_objective_terms());
        assert_eq!(problem.problem_type, ProblemType::QuadraticContinuous);

        // Check that clearing the objective set the problem type correctly
        problem.remove_all_objective_terms();
        assert!(!problem.has_integer_variables());
        assert!(!problem.has_quadratic_objective_terms());
        assert_eq!(problem.problem_type, ProblemType::LinearContinuous);
    }

    fn get_test_problem() -> Problem {
        let mut problem = Problem::new(ObjectiveSense::Maximize);
        problem
            .add_new_variable("x", None, VariableType::Continuous, 64., 100.)
            .unwrap();
        problem
            .add_new_variable("y", None, VariableType::Continuous, 64., 100.)
            .unwrap();

        problem
            .add_new_equality_constraint("test_equality_constraint", &["x", "y"], &[4., 5.], 5.)
            .unwrap();
        problem
            .add_new_inequality_constraint(
                "test_inequality_constraint",
                &["x", "y"],
                &[2., 5.],
                1.,
                10.,
            )
            .unwrap();
        problem.add_new_linear_objective_term("y", 12.).unwrap();
        problem
    }
}
