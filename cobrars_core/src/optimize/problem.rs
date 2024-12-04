//! Provides struct representing an optimization problem
use crate::optimize::constraint::Constraint;
use crate::optimize::objective::{Objective, ObjectiveSense};
use crate::optimize::solvers::Solver;
use crate::optimize::variable::{Variable, VariableBuilder, VariableType};
use indexmap::IndexMap;
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

    // region Adding Variables
    /// Add a variable to the optimization problem
    pub fn add_variable(&mut self, variable: Arc<RwLock<Variable>>)-> Result<(), ProblemError> {
        // Validate that the variable can in fact be added to the problem
        self.validate_variable(variable.clone())?;
        // Update the index of the variable to reflect the current variable count
        variable.write().unwrap().index = self.num_variables;
        // Update the total number of variables
        self.num_variables += 1;
        // Insert the variable into the variables IndexMap 
        let var_id = variable.read().unwrap().id.clone();
        self.variables.insert(var_id, variable);
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
    )-> Result<(), ProblemError> {
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
    pub fn add_constraint(&mut self, constraint: Arc<RwLock<Constraint>>) -> Result<(), ProblemError> {
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
    ) {
        let new_cons = Constraint::new_equality(
            id, variables, coefficients, equals,
        ).wrap();
        self.add_constraint(new_cons);
    }

    /// Create a new equality constraint using variable ids rather than variables references, and add it to the model
    pub fn add_new_equality_constraint_by_id(
        &mut self,
        id: &str,
        variables: &[&str],
        coefficients: &[f64],
        equals: f64,
    ) {
        let variables = variables.iter().map(|v_id| self.variables.get(*v_id).unwrap().clone()).collect::<Vec<_>>();
        self.add_new_equality_constraint(id, &variables, coefficients, equals);
    }
    // endregion Adding Constraints
    
    // region Validation Functions
    /// Check that a variable to be added is valid to add to this problem
    fn validate_variable(&self, variable: Arc<RwLock<Variable>>)->Result<(), ProblemError> {
        // Check if there is already a variable with this id
        if self.variables.get(&variable.read().unwrap().id).is_some() {
            return Err(ProblemError::VariableIdAlreadyExists);
        };
        // Check if the variable type conflicts with the problem type
        match variable.read().unwrap().variable_type {
            VariableType::Binary | VariableType::Integer=> {
                // Trying to add an integer variable, need to make sure problem
                // objective is not quadratic
                if let ProblemType::QuadraticContinuous = self.problem_type {
                    return Err(ProblemError::AddingIntegerVariableWithQuadraticObjective)
                }
            }
            _ => {}
        }
        // Check if the variable bounds are valid
        let lb = variable.read().unwrap().lower_bound;
        let ub = variable.read().unwrap().upper_bound;
        if lb > ub {
            return Err(ProblemError::InvalidVariableBounds)
        }
        Ok(())
    }
    
    fn validate_constraint(&self, constraint: Arc<RwLock<Constraint>>)->Result<(), ProblemError> {
        // Check that a variable with the same id doesn't already exist
        if self.constraints.get(&constraint.read().unwrap().get_id()).is_some(){
            return Err(ProblemError::ConstraintAlreadyExists);
        }        
        // Check that for inequality constraints the bounds make sense
        match *constraint.read().unwrap() {
            Constraint::Equality { .. } => {}
            Constraint::Inequality { lower_bound, upper_bound, .. } => {
                if lower_bound > upper_bound {
                    return Err(ProblemError::InvalidConstraintBounds);
                }
            }
        }
        Ok(())
    }
    
    // endregion Validation Functions
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
    variable_values: Option<IndexMap<Variable, f64>>,
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
#[derive(Clone, Debug)]
pub enum ProblemType {
    /// Problem with linear objectives and constraints, and continuous variables
    LinearContinuous,
    /// Problem with quadratic objective, linear constraints, and continuous variables
    QuadraticContinuous,
    /// Problem with linear objective and constraints, with integer and continuous variables
    LinearMixedInteger,
}

/// Errors associated with the Problem
#[derive(Error, Debug, Clone)]
pub enum ProblemError {
    /// Error when trying to add an integer variable to a problem with a quadratic objective 
    #[error("Tried added integer variable to problem with quadratic objective")]
    AddingIntegerVariableWithQuadraticObjective,
    /// Error when trying to add a quadratic term to the objective when problem contains integer variables
    #[error("Tried adding quadratic objective to problem containing integer variables")]
    AddingQuadraticObjectiveWithIntegerVariable, 
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
}