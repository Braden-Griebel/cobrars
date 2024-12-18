//! This module provides the Model struct for representing an entire metabolic model

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};

use indexmap::IndexMap;
use thiserror::Error;

use crate::configuration::CONFIGURATION;
use crate::io::gpr_parse::{parse_gpr, GprParseError};
use crate::metabolic_model::gene::{Gene, GeneActivity};
use crate::metabolic_model::metabolite::Metabolite;
use crate::metabolic_model::reaction::{Reaction, ReactionActivity};
use crate::optimize::problem::{Problem, ProblemError};
use crate::optimize::solvers::clarabel::ClarabelSolver;
use crate::optimize::solvers::{SelectedSolver, Solver};
use crate::optimize::variable::VariableType;
use crate::optimize::{OptimizationStatus, ProblemSolution};

/// Represents a Genome Scale Metabolic Model
#[derive(Clone, Debug)]
pub struct Model {
    /// Map of reaction ids to Reaction Objects (which are wrapped in Arc<RwLock<>>)
    pub reactions: IndexMap<String, Reaction>,
    /// Map of gene ids to Gene Objects (which are wrapped in Arc<RwLock<>>)
    pub genes: IndexMap<String, Gene>,
    /// Map of metabolite ids to Metabolite Objects (which are wrapped in Arc<RwLock<>>)
    pub metabolites: IndexMap<String, Metabolite>,
    /// Map of reaction ids to objective function coefficients
    pub objective: IndexMap<String, f64>,
    /// Underlying optimization problem
    pub problem: Option<Problem>,
    /// ID associated with the Model
    pub id: Option<String>,
    /// Compartments in the model
    ///
    /// An IndexMap<String, String> of {short name: long name}
    pub compartments: Option<IndexMap<String, String>>,
    /// A version identifier for the Model, stored as a string
    pub version: Option<String>,
    /// The backend solver to use for solving this model
    pub solver: SelectedSolver,
    /// A flag to determine if reaction activity may have changed
    pub(crate) reaction_activity_update_required: bool,
}

impl Model {
    /// Create a new empty model
    pub fn new_empty() -> Self {
        let solver = CONFIGURATION.read().unwrap().solver.clone();
        Model {
            reactions: IndexMap::new(),
            genes: IndexMap::new(),
            metabolites: IndexMap::new(),
            objective: IndexMap::new(),
            problem: None,
            id: None,
            compartments: None,
            version: None,
            solver,
            reaction_activity_update_required: false,
        }
    }
}

/// Functions for updating a model
impl Model {
    /// Add a reaction to the model
    ///
    /// # Parameters
    /// - reaction: Reaction to add
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::reaction::{Reaction, ReactionBuilder};
    /// let mut model = Model::new_empty();
    /// let new_reaction = ReactionBuilder::default().id("new_reaction".to_string()).build().unwrap();
    /// model.add_reaction(new_reaction);
    /// ```
    pub fn add_reaction(&mut self, mut reaction: Reaction) -> Result<(), ModelError> {
        let id = reaction.id.clone();
        // If needed, parse the GPR string of the reaction
        if let Some(ref gpr_string) = reaction.gpr_string {
            reaction.gpr = Some(parse_gpr(gpr_string, &mut self.genes)?);
            reaction.gpr_string = None;
        }
        self.reactions.insert(id, reaction);
        Ok(())
    }

    /// Remove a reaction from the model
    ///
    /// See [`self.knockout_reaction`] for a method which temporarily disables a reaction
    /// rather than removing it.
    ///
    /// # Parameters
    /// - id: ID of the reaction to remove
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::reaction::{Reaction, ReactionBuilder};
    /// let mut model = Model::new_empty();
    /// let new_reaction = ReactionBuilder::default().id("new_reaction".to_string()).build().unwrap();
    /// model.add_reaction(new_reaction);
    /// model.remove_reaction("new_reaction").unwrap();
    /// ```
    pub fn remove_reaction(&mut self, id: &str) -> Result<(), ModelError> {
        if self.reactions.shift_remove(id).is_none() {
            return Err(ModelError::InvalidReaction {
                message: format!(
                    "Tried to remove reaction with id {}, but it is not present in model",
                    id
                ),
            });
        };
        Ok(())
    }

    /// Add a gene to the model
    ///
    /// # Parameters
    /// - gene: Gene to add
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::gene::GeneBuilder;
    /// use cobrars_core::metabolic_model::model::Model;
    /// let mut model=Model::new_empty();
    /// let new_gene = GeneBuilder::default().id("new_gene".to_string()).build().unwrap();
    /// model.add_gene(new_gene);
    /// ```
    pub fn add_gene(&mut self, gene: Gene) {
        let id = gene.id.clone();
        self.genes.insert(id, gene);
    }

    /// Remove a gene from the model
    ///
    /// Note: This will not remove the gene from all reactions which may include the gene in
    /// their GPRs, which can result in an inconsistent model. See [`self.knockout_gene`] for
    /// a method which temporarily disables a gene rather than removing it.
    ///
    /// # Parameters
    /// - gene: Gene to add
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::gene::GeneBuilder;
    /// use cobrars_core::metabolic_model::model::Model;
    /// let mut model=Model::new_empty();
    /// let new_gene = GeneBuilder::default().id("new_gene".to_string()).build().unwrap();
    /// model.add_gene(new_gene);
    /// ```
    pub fn remove_gene(&mut self, id: &str) -> Result<(), ModelError> {
        if self.genes.shift_remove(id).is_none() {
            return Err(ModelError::InvalidGene {
                message: format!(
                    "Tried to remove gene with id {}, but it is not present in model",
                    id
                ),
            });
        }
        Ok(())
    }

    /// Add a metabolite to the model
    ///
    /// # Parameters
    /// - metabolite: Metabolite to add
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::metabolite::MetaboliteBuilder;
    /// use cobrars_core::metabolic_model::model::Model;
    /// let mut model=Model::new_empty();
    /// let new_metabolite = MetaboliteBuilder::default().id("new_metabolite".to_string()).build().unwrap();
    /// model.add_metabolite(new_metabolite);
    /// ```
    pub fn add_metabolite(&mut self, metabolite: Metabolite) {
        let id = metabolite.id.clone();
        self.metabolites.insert(id, metabolite);
    }

    /// Remove a metabolite from the model
    ///
    /// # Parameters
    /// - metabolite: Metabolite to remove
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::metabolite::MetaboliteBuilder;
    /// use cobrars_core::metabolic_model::model::Model;
    /// let mut model=Model::new_empty();
    /// let new_metabolite = MetaboliteBuilder::default().id("new_metabolite".to_string()).build().unwrap();
    /// model.add_metabolite(new_metabolite);
    /// model.remove_metabolite("new_metabolite").unwrap()
    /// ```
    pub fn remove_metabolite(&mut self, id: &str) -> Result<(), ModelError> {
        if self.metabolites.shift_remove(id).is_none() {
            return Err(ModelError::InvalidMetabolite {
                message: format!(
                    "Tried to remove metabolite with id {}, but it is not present in model",
                    id
                ),
            });
        };
        Ok(())
    }

    /// Knock out a gene
    ///
    /// Sets a particular gene to be inactive
    ///
    /// # Parameters
    /// - gene: Gene to knock out
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::gene::GeneBuilder;
    /// let mut model=Model::new_empty();
    /// let new_gene = GeneBuilder::default().id("new_gene".to_string()).build().unwrap();
    /// model.add_gene(new_gene);
    /// model.knockout_gene("new_gene").unwrap()
    /// ```
    pub fn knockout_gene(&mut self, gene: &str) -> Result<(), ModelError> {
        let gene = match self.genes.get_mut(gene) {
            Some(gene) => gene,
            None => {
                return Err(ModelError::InvalidGene {
                    message: format!(
                        "Tried to knock out gene {}, but it is not present in model",
                        gene
                    ),
                })
            }
        };
        gene.activity = GeneActivity::Inactive;
        self.reaction_activity_update_required = true;
        Ok(())
    }

    /// Enable a gene
    ///
    /// Re-enables a gene which was previously knocked out
    ///
    /// # Parameters
    /// - gene: Gene to enable
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::gene::GeneBuilder;
    /// let mut model=Model::new_empty();
    /// let new_gene = GeneBuilder::default().id("new_gene".to_string()).build().unwrap();
    /// model.add_gene(new_gene);
    /// model.knockout_gene("new_gene").unwrap();
    /// model.enable_gene("new_gene").unwrap();
    /// ```
    pub fn enable_gene(&mut self, gene: &str) -> Result<(), ModelError> {
        let gene = match self.genes.get_mut(gene) {
            Some(gene) => gene,
            None => {
                return Err(ModelError::InvalidGene {
                    message: format!(
                        "Tried to enable gene {}, but it is not present in model",
                        gene
                    ),
                })
            }
        };
        gene.activity = GeneActivity::Active;
        self.reaction_activity_update_required = true;
        Ok(())
    }

    /// Temporarily disable a reaction
    ///
    /// See [`self.remove_reaction`] if you want to permanently remove a reaction,
    /// [`self.enable_reaction`] to undo this knockout, and [`self.force_active_reaction`] to
    /// force a reaction to be active.
    ///
    /// # Parameters
    /// - reaction: Reaction to temporarily disable
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::reaction::{Reaction, ReactionBuilder};
    /// let mut model = Model::new_empty();
    /// let new_reaction = ReactionBuilder::default().id("new_reaction".to_string()).build().unwrap();
    /// model.add_reaction(new_reaction);
    /// model.knockout_reaction("new_reaction").unwrap();
    /// ```
    pub fn knockout_reaction(&mut self, reaction: &str) -> Result<(), ModelError> {
        let rxn = match self.reactions.get_mut(reaction) {
            Some(rxn) => rxn,
            None => {
                return Err(ModelError::InvalidReaction {
                    message: format!(
                        "Tried to knock out reaction {}, but it is not present in model",
                        reaction
                    ),
                })
            }
        };

        rxn.activity = ReactionActivity::Inactive;
        rxn.activity_set = true;
        Ok(())
    }

    /// Enable a disabled reaction, undoing [`self.knockout_reaction`] or [`self.force_active_reaction`]
    ///
    /// See [`self.remove_reaction`] if you want to permanently remove a reaction,
    /// [`self.knockout_reaction`] to temporarily disable a reaction, and [`self.force_active_reaction`] to
    /// force a reaction to be active.
    ///
    /// # Parameters
    /// - reaction: Reaction to temporarily disable
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::reaction::{Reaction, ReactionBuilder};
    /// let mut model = Model::new_empty();
    /// let new_reaction = ReactionBuilder::default().id("new_reaction".to_string()).build().unwrap();
    /// model.add_reaction(new_reaction);
    /// // Can re-enable a reaction which is temporarily knocked out
    /// model.knockout_reaction("new_reaction").unwrap();
    /// model.enable_reaction("new_reaction").unwrap();
    /// // Can also undo force_active_reaction
    /// model.force_active_reaction("new_reaction").unwrap();
    /// model.enable_reaction("new_reaction").unwrap();
    /// ```
    pub fn enable_reaction(&mut self, reaction: &str) -> Result<(), ModelError> {
        let rxn = match self.reactions.get_mut(reaction) {
            Some(rxn) => rxn,
            None => {
                return Err(ModelError::InvalidReaction {
                    message: format!(
                        "Tried to enable reaction {}, but it is not present in model",
                        reaction
                    ),
                })
            }
        };
        rxn.activity = ReactionActivity::Active;
        rxn.activity_set = false;
        self.reaction_activity_update_required = true;

        Ok(())
    }

    /// Force a reaction to be active, the opposite of [`self.knockout_reaction`]
    ///
    /// See [`self.remove_reaction`] if you want to permanently remove a reaction,
    /// [`self.knockout_reaction`] to temporarily disable a reaction, and [`self.enable_reaction`] to
    /// remove activity restrictions (making the reaction rely on the associated GPR to determine
    /// activity).
    ///
    /// # Parameters
    /// - reaction: Reaction to temporarily disable
    ///
    /// # Examples
    /// ```rust
    /// use cobrars_core::metabolic_model::model::Model;
    /// use cobrars_core::metabolic_model::reaction::{Reaction, ReactionBuilder};
    /// let mut model = Model::new_empty();
    /// let new_reaction = ReactionBuilder::default().id("new_reaction".to_string()).build().unwrap();
    /// model.add_reaction(new_reaction);
    /// model.force_active_reaction("new_reaction").unwrap();
    /// ```
    pub fn force_active_reaction(&mut self, reaction: &str) -> Result<(), ModelError> {
        let rxn = match self.reactions.get_mut(reaction) {
            Some(rxn) => rxn,
            None => {
                return Err(ModelError::InvalidReaction {
                    message: format!(
                        "Tried to force on reaction {}, but it is not present in model",
                        reaction
                    ),
                })
            }
        };
        rxn.activity = ReactionActivity::Active;
        rxn.activity_set = true;

        Ok(())
    }
}

/// Presents the solution to the optimization problem constructed from the Model
///
/// Differs from the ProblemSolution in that the reaction fluxes and metabolite
/// shadow costs have been gathered.
pub struct ModelSolution {
    /// The status of the optimization problem, representing if the
    /// optimization was completed successfully
    status: OptimizationStatus,
    /// Optimized value of the objective
    ///
    /// Some(f64) if the optimization was completed successfully, None otherwise
    objective_value: Option<f64>,
    /// The fluxes for individual reaction in the Model
    ///
    /// Some(IndexMap), keyed by reaction id, with values corresponding to the
    /// flux values at optimum if the problem was solved successfully, None otherwise
    reaction_fluxes: Option<IndexMap<String, f64>>,
    /// The shadow prices of the metabolites
    ///
    /// Some(IndexMap) keyed by metabolite id, with values corresponding to the dual variable
    /// values associated with the metabolite constraints at the optimum if the problem could be
    /// solved, and the solver supports retrieving dual values, None otherwise
    metabolite_shadow_prices: Option<IndexMap<String, f64>>,
    /// Represents the solution to the underlying optimization problem, without
    /// any post-processing
    problem_solution: ProblemSolution,
}

/// Functions for constructing an optimization problem from a Model
impl Model {
    pub fn optimize(&mut self) -> Result<ModelSolution, ModelError> {
        if self.problem.is_none() {
            self.generate_problem();
        }

        let problem_solution = match self.problem {
            None => return Err(ModelError::ProblemGenerationError),
            Some(ref mut prob) => match self.solver {
                SelectedSolver::Clarabel => {
                    let mut solver = ClarabelSolver::new();
                    prob.solve(&mut solver)?
                }
                SelectedSolver::Scip => {
                    todo!()
                }
                SelectedSolver::Osqp => {
                    todo!()
                }
            },
        };

        // If the problem was successfully solved
        if let OptimizationStatus::Optimal = problem_solution.status {
            // gather the reaction fluxes
            let mut reaction_fluxes = IndexMap::<String, f64>::new();
            for (rxn_id, rxn) in self.reactions.iter() {
                let (forward_flux, reverse_flux) = match problem_solution.variable_values {
                    None => {
                        return Err(ModelError::InvalidSolution {
                            message: "ProblemSolution missing variable values".to_string(),
                        })
                    }
                    Some(ref map) => {
                        let forward_flux = match map.get(&rxn.get_forward_id()) {
                            None => {
                                return Err(ModelError::InvalidSolution {
                                    message: format!(
                                        "Error gathering forward flux value for reaction {}",
                                        rxn_id
                                    )
                                    .to_string(),
                                })
                            }
                            Some(value) => value.clone(),
                        };
                        let reverse_flux = match map.get(&rxn.get_reverse_id()) {
                            None => {
                                return Err(ModelError::InvalidSolution {
                                    message: format!(
                                        "Error gathering reverse flux value for reaction {}",
                                        rxn_id
                                    )
                                    .to_string(),
                                })
                            }
                            Some(value) => value.clone(),
                        };
                        (forward_flux, reverse_flux)
                    }
                };
                reaction_fluxes.insert(rxn_id.clone(), forward_flux - reverse_flux);
            }
            // gather the shadow prices if available
            let metabolite_shadow_prices = match problem_solution.dual_values {
                None => None,
                Some(ref map) => {
                    let mut metabolite_shadow_prices = IndexMap::<String, f64>::new();
                    for metabolite_id in self.metabolites.keys() {
                        if let Some(value) = map.get(metabolite_id) {
                            metabolite_shadow_prices.insert(metabolite_id.clone(), value.clone());
                        }
                    }
                    Some(metabolite_shadow_prices)
                }
            };

            Ok(ModelSolution {
                status: OptimizationStatus::Unoptimized,
                objective_value: problem_solution.objective_value,
                reaction_fluxes: Some(reaction_fluxes),
                metabolite_shadow_prices,
                problem_solution,
            })
        } else {
            // The problem wasn't solved, everything is None
            Ok(ModelSolution {
                status: problem_solution.status,
                objective_value: None,
                reaction_fluxes: None,
                metabolite_shadow_prices: None,
                problem_solution,
            })
        }
    }

    pub fn generate_problem(&mut self) -> Result<(), ModelError> {
        // Ensure the reaction activity matches the current state of gene activity
        if self.reaction_activity_update_required {
            self.update_reaction_activity();
        }
        // Create a problem from the model
        self.problem = Some(self.problem_from_model()?);
        Ok(())
    }

    /// Update reaction activity based on current state of genes
    fn update_reaction_activity(&mut self) -> Result<(), ModelError> {
        let mut reaction_activity: VecDeque<ReactionActivity> = VecDeque::new();
        // Determine the activity of all the reactions
        for reaction in self.reactions.values() {
            if reaction.activity_set {
                reaction_activity.push_back(reaction.activity.clone());
                continue;
            }
            match reaction.gpr {
                None => {
                    reaction_activity.push_back(ReactionActivity::Active);
                }
                Some(ref gpr) => reaction_activity.push_back(self.eval_gpr(gpr)?.into()),
            }
        }
        // Now actually update the activities
        for (reaction, activity) in self
            .reactions
            .values_mut()
            .zip(reaction_activity.into_iter())
        {
            reaction.activity = activity;
        }
        // Since the activity has been updated, the reaction activities are correct and no longer need to be updated
        self.reaction_activity_update_required = false;
        Ok(())
    }

    /// Create an optimization problem from the Model
    fn problem_from_model(&self) -> Result<Problem, ModelError> {
        // Create problem, with maximization as the default
        let mut associated_problem = Problem::new_maximization();
        // An index map to hold the constraints associated with each metabolite
        let mut metabolite_constraints: IndexMap<String, (Vec<String>, Vec<f64>)> = IndexMap::new();
        for met in self.metabolites.values() {
            metabolite_constraints.insert(met.id.clone(), (Vec::new(), Vec::new()));
        }

        /* For every reaction, add the appropriate variables to the problem,
        and add the variable to the appropriate metabolite constraints*/
        for (_, rxn) in self.reactions.iter() {
            // Add the reaction variables to the problem
            // Forward variable
            associated_problem.add_new_variable(
                &rxn.get_forward_id(),
                None,
                VariableType::Continuous,
                rxn.get_forward_lower_bound(),
                rxn.get_forward_upper_bound(),
            )?;
            // Reverse variable
            associated_problem.add_new_variable(
                &rxn.get_reverse_id(),
                None,
                VariableType::Continuous,
                rxn.get_reverse_lower_bound(),
                rxn.get_reverse_upper_bound(),
            )?;
            // Add the reactions to the appropriate metabolite constraints
            for (met, coef) in rxn.metabolites.iter() {
                metabolite_constraints
                    .entry(met.clone())
                    .and_modify(|(rxns, coefs)| {
                        // Add the forward variable and coefficient
                        rxns.push(rxn.get_forward_id());
                        coefs.push(*coef);
                        // Add the reverse variable and coefficient
                        rxns.push(rxn.get_reverse_id());
                        coefs.push(-1f64 * *coef);
                    });
            }
        }

        // Add constraints to the problem
        for (met_id, (var_ids, var_coefs)) in metabolite_constraints.iter() {
            associated_problem.add_new_equality_constraint(
                met_id,
                &var_ids.iter().map(AsRef::as_ref).collect::<Vec<&str>>(),
                var_coefs,
                0f64,
            )?
        }

        // Add the objective
        for (rxn_id, coef) in self.objective.iter() {
            let rxn = match self.reactions.get(rxn_id) {
                Some(r) => r,
                None => {
                    return Err(ModelError::InvalidReaction {
                        message: format!("Invalid reaction in objective: {}", rxn_id).to_string(),
                    })
                }
            };
            let fwd_id = rxn.get_forward_id();
            let rev_id = rxn.get_reverse_id();
            associated_problem.add_new_linear_objective_term(&fwd_id, *coef)?;
            associated_problem.add_new_linear_objective_term(&rev_id, -*coef)?;
        }

        Ok(associated_problem)
    }
}

// region Model Error
#[derive(Error, Debug, Clone)]
pub enum ModelError {
    #[error("Problem from associated problem: {0}")]
    AssociatedProblemError(#[from] ProblemError),
    #[error("Problem accessing metabolite: {message}")]
    InvalidMetabolite { message: String },
    #[error("Problem accessing reaction: {message}")]
    InvalidReaction { message: String },
    #[error("Problem accessing gene: {message}")]
    InvalidGene { message: String },
    #[error("Model failed to generate optimization problem")]
    ProblemGenerationError,
    #[error("Problem accessing reaction activity: {0}")]
    GprError(#[from] GprError),
    #[error("Problem parsing a gene protein reaction string: {0}")]
    GprParseError(#[from] GprParseError),
    #[error("Problem generating model solution from problem solution: {message}")]
    InvalidSolution { message: String },
}
// endregion Model Error

// region GPR Functionality
/// Representation of a Gene Protein Reaction Rule as an AST
#[derive(Clone, Debug)]
pub enum Gpr {
    /// Operation on two genes (see [`GprOperation`])
    Operation(GprOperation),
    /// A terminal gene Node (see [`Gene`])
    GeneNode(String),
}

impl Display for Gpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_id())
    }
}

impl Gpr {
    /// Create a new binary operation node
    pub fn new_binary_operation(
        left: Gpr,
        operator: GprOperatorType,
        right: Gpr,
    ) -> Result<Gpr, GprError> {
        let op = match operator {
            GprOperatorType::Or => GprOperation::Or {
                left: Box::new(left),
                right: Box::new(right),
            },
            GprOperatorType::And => GprOperation::And {
                left: Box::new(left),
                right: Box::new(right),
            },
            GprOperatorType::Not => return Err(GprError::InvalidBinaryOp),
        };
        Ok(Gpr::Operation(op))
    }

    /// Create a new unary operation node
    pub fn new_unary_operation(operator: GprOperatorType, operand: Gpr) -> Result<Gpr, GprError> {
        let op = match operator {
            GprOperatorType::Not => GprOperation::Not {
                val: Box::new(operand),
            },
            _ => return Err(GprError::InvalidUnaryOp),
        };
        Ok(Gpr::Operation(op))
    }

    /// Create a new gene node
    pub fn new_gene_node(gene: &str) -> Gpr {
        Gpr::GeneNode(gene.to_string())
    }

    /// Generate a GPR string with gene ids from the GPR AST
    pub fn to_string_id(&self) -> String {
        match self {
            Gpr::Operation(op) => match op {
                GprOperation::Or { left, right } => {
                    format!("({} or {})", left.to_string_id(), right.to_string_id())
                }
                GprOperation::And { left, right } => {
                    format!("({} and {})", left.to_string_id(), right.to_string_id())
                }
                GprOperation::Not { val } => {
                    format!("(not {})", val)
                }
            },
            Gpr::GeneNode(gene_ref) => gene_ref.to_string(),
        }
    }
}

/// Possible operations on genes
#[derive(Clone, Debug)]
pub enum GprOperation {
    Or { left: Box<Gpr>, right: Box<Gpr> },
    And { left: Box<Gpr>, right: Box<Gpr> },
    Not { val: Box<Gpr> },
}

/// Types of Allowed GPR Operations
pub enum GprOperatorType {
    /// Or, results in active if either left or right are active
    Or,
    /// And, results in active if both left and right are active
    And,
    /// Not, results in active if val is inactive
    Not,
}

#[derive(Clone, Debug, Error)]
pub enum GprError {
    #[error("Invalid Binary Operation")]
    InvalidBinaryOp,
    #[error("Invalid Unary Operation")]
    InvalidUnaryOp,
    #[error("Gene is GPR is not present in the model")]
    GeneNotFound,
}

// Model associated functions for working with GPRs
impl Model {
    /// Evaluate whether a GPR evaluates to Active or Inactive
    pub fn eval_gpr(&self, gpr: &Gpr) -> Result<GeneActivity, GprError> {
        match gpr {
            Gpr::Operation(op) => match op {
                GprOperation::Or { left, right } => {
                    let l = self.eval_gpr(&(*left))?;
                    let r = self.eval_gpr(&(*right))?;
                    if l == GeneActivity::Active || r == GeneActivity::Active {
                        Ok(GeneActivity::Active)
                    } else {
                        Ok(GeneActivity::Inactive)
                    }
                }
                GprOperation::And { left, right } => {
                    let l = self.eval_gpr(&(*left))?;
                    let r = self.eval_gpr(&(*right))?;
                    if l == GeneActivity::Active && r == GeneActivity::Active {
                        Ok(GeneActivity::Active)
                    } else {
                        Ok(GeneActivity::Inactive)
                    }
                }
                GprOperation::Not { val } => match self.eval_gpr(&(*val))? {
                    GeneActivity::Active => Ok(GeneActivity::Inactive),
                    GeneActivity::Inactive => Ok(GeneActivity::Active),
                },
            },
            Gpr::GeneNode(gene) => match self.genes.get(gene) {
                Some(g) => Ok(g.activity.clone()),
                None => Err(GprError::GeneNotFound),
            },
        }
    }
}

// endregion GPR Functionality

#[cfg(test)]
mod gpr_tests {
    use super::*;
    use crate::metabolic_model::gene::GeneBuilder;
    use indexmap::IndexMap;

    fn setup_model() -> Model {
        let mut model = Model::new_empty();
        // This model only needs to hold genes for these tests
        // Create some active genes
        let active_gene1 = GeneBuilder::default()
            .id("active_gene1".to_string())
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let active_gene2 = GeneBuilder::default()
            .id("active_gene2".to_string())
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        // Create some inactive genes
        let inactive_gene1 = GeneBuilder::default()
            .id("inactive_gene1".to_string())
            .activity(GeneActivity::Inactive)
            .build()
            .unwrap();
        let inactive_gene2 = GeneBuilder::default()
            .id("inactive_gene2".to_string())
            .activity(GeneActivity::Inactive)
            .build()
            .unwrap();
        // Add these genes to the model
        model.add_gene(active_gene1);
        model.add_gene(active_gene2);
        model.add_gene(inactive_gene1);
        model.add_gene(inactive_gene2);

        model
    }

    #[test]
    fn gene_node() {
        let model = setup_model();
        let active_gene_node = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node = Gpr::GeneNode("inactive_gene1".to_string());
        assert_eq!(
            model.eval_gpr(&active_gene_node).unwrap(),
            GeneActivity::Active
        );
        assert_eq!(
            model.eval_gpr(&inactive_gene_node).unwrap(),
            GeneActivity::Inactive
        );
    }

    #[test]
    fn and_node() {
        let model = setup_model();
        // First with two active genes
        let active_gene_node1 = Gpr::GeneNode("active_gene1".to_string());
        let active_gene_node2 = Gpr::GeneNode("active_gene2".to_string());
        let gpr_and_active = Gpr::Operation(GprOperation::And {
            left: Box::new(active_gene_node1.clone()),
            right: Box::new(active_gene_node2.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_and_active).unwrap(),
            GeneActivity::Active
        );

        // With an active and an inactive gene
        let active_gene_node = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node = Gpr::GeneNode("inactive_gene1".to_string());
        let gpr_and_inactive = Gpr::Operation(GprOperation::And {
            left: Box::new(active_gene_node.clone()),
            right: Box::new(inactive_gene_node.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_and_inactive).unwrap(),
            GeneActivity::Inactive
        );

        // Finally with two inactive genes
        let inactive_gene_node1 = Gpr::GeneNode("inactive_gene1".to_string());
        let inactive_gene_node2 = Gpr::GeneNode("inactive_gene2".to_string());
        let gpr_and_inactive = Gpr::Operation(GprOperation::And {
            left: Box::new(inactive_gene_node1.clone()),
            right: Box::new(inactive_gene_node2.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_and_inactive).unwrap(),
            GeneActivity::Inactive
        );
    }

    #[test]
    fn or_node() {
        let model = setup_model();
        // First with two active genes
        let active_gene_node1 = Gpr::GeneNode("active_gene1".to_string());
        let active_gene_node2 = Gpr::GeneNode("active_gene2".to_string());
        let gpr_or_active = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene_node1.clone()),
            right: Box::new(active_gene_node2.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_or_active).unwrap(),
            GeneActivity::Active
        );

        // With an active or an inactive gene
        let active_gene_node = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node = Gpr::GeneNode("inactive_gene1".to_string());
        let gpr_or_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene_node.clone()),
            right: Box::new(inactive_gene_node.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_or_inactive).unwrap(),
            GeneActivity::Active
        );

        // Finally with two inactive genes
        let inactive_gene_node1 = Gpr::GeneNode("inactive_gene1".to_string());
        let inactive_gene_node2 = Gpr::GeneNode("inactive_gene2".to_string());
        let gpr_or_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(inactive_gene_node1.clone()),
            right: Box::new(inactive_gene_node2.clone()),
        });

        assert_eq!(
            model.eval_gpr(&gpr_or_inactive).unwrap(),
            GeneActivity::Inactive
        );
    }

    #[test]
    fn not_node() {
        let model = setup_model();

        let active_gene_node1 = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node1 = Gpr::GeneNode("inactive_gene1".to_string());

        // First check not active
        let gpr_not_inactive = Gpr::Operation(GprOperation::Not {
            val: Box::new(active_gene_node1),
        });
        assert_eq!(
            model.eval_gpr(&gpr_not_inactive).unwrap(),
            GeneActivity::Inactive
        );

        // Next check the inactive
        let gpr_not_active = Gpr::Operation(GprOperation::Not {
            val: Box::new(inactive_gene_node1),
        });
        assert_eq!(
            model.eval_gpr(&gpr_not_active).unwrap(),
            GeneActivity::Active
        );
    }

    #[test]
    fn display() {
        // Test single gene display
        let active_gene_node = Gpr::GeneNode("ActiveGene1".to_string());
        assert_eq!(format!("{}", active_gene_node), "ActiveGene1");

        // Test and gene display
        let active_gene1_node = Gpr::GeneNode("Active1".to_string());
        let active_gene2_node = Gpr::GeneNode("Active2".to_string());
        let gpr_or_active = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene1_node),
            right: Box::new(active_gene2_node),
        });
        assert_eq!(format!("{}", gpr_or_active), "(Active1 or Active2)");

        // Test nested with parsing
        use crate::io::gpr_parse::parse_gpr;
        let rv0001 = GeneBuilder::default()
            .id("Rv0001".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0002 = GeneBuilder::default()
            .id("Rv0002".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0003 = GeneBuilder::default()
            .id("Rv0003".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let mut gene_map = IndexMap::new();
        gene_map.insert("Rv0001".to_string(), rv0001);
        gene_map.insert("Rv0002".to_string(), rv0002);
        gene_map.insert("Rv0003".to_string(), rv0003);
        let gpr = parse_gpr("(Rv0001 and Rv0002) or Rv0003", &mut gene_map).unwrap();
        // Note, because of how the display works, it will be very explicit with parenthesis
        // so an extra pair will be wrapped around the entire expression
        assert_eq!(format!("{}", gpr), "((Rv0001 and Rv0002) or Rv0003)");

        // Test chained binary operations
        let rv0001 = GeneBuilder::default()
            .id("Rv0001".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0002 = GeneBuilder::default()
            .id("Rv0002".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0003 = GeneBuilder::default()
            .id("Rv0003".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let mut gene_map = IndexMap::new();
        gene_map.insert("Rv0001".to_string(), rv0001);
        gene_map.insert("Rv0002".to_string(), rv0002);
        gene_map.insert("Rv0003".to_string(), rv0003);
        let gpr = parse_gpr("Rv0001 and Rv0002 or Rv0003", &mut gene_map).unwrap();
        // Note, because of how the display works, it will be very explicit with parenthesis
        // so an extra pair will be wrapped around the entire expression
        assert_eq!(format!("{}", gpr), "((Rv0001 and Rv0002) or Rv0003)");

        // Test with Not
        let rv0001 = GeneBuilder::default()
            .id("Rv0001".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0002 = GeneBuilder::default()
            .id("Rv0002".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let rv0003 = GeneBuilder::default()
            .id("Rv0003".to_string())
            .annotation(None)
            .activity(GeneActivity::Active)
            .build()
            .unwrap();
        let mut gene_map = IndexMap::new();
        gene_map.insert("Rv0001".to_string(), rv0001);
        gene_map.insert("Rv0002".to_string(), rv0002);
        gene_map.insert("Rv0003".to_string(), rv0003);
        let gpr = parse_gpr("(Rv0001 and not Rv0002) or not Rv0003", &mut gene_map).unwrap();
        // Note, because of how the display works, it will be very explicit with parenthesis
        // so an extra pair will be wrapped around the entire expression
        assert_eq!(
            format!("{}", gpr),
            "((Rv0001 and (not Rv0002)) or (not Rv0003))"
        );
    }
}

#[cfg(test)]
mod model_tests {
    use super::*;
    use crate::metabolic_model::gene::GeneBuilder;
    use crate::metabolic_model::metabolite::MetaboliteBuilder;
    use crate::metabolic_model::reaction::ReactionBuilder;

    fn set_up_small_model() -> Model {
        let mut test_model = Model::new_empty();
        // region Create Metabolites
        let a_e = MetaboliteBuilder::default()
            .id("A_e".to_string())
            .name(Some("A".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C11H21N2".to_string()))
            .build()
            .unwrap();
        let b_e = MetaboliteBuilder::default()
            .id("B_e".to_string())
            .name(Some("B".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let c_e = MetaboliteBuilder::default()
            .id("C_e".to_string())
            .name(Some("C".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let f_e = MetaboliteBuilder::default()
            .id("F_e".to_string())
            .name(Some("F".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let g_e = MetaboliteBuilder::default()
            .id("G_e".to_string())
            .name(Some("G".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let h_e = MetaboliteBuilder::default()
            .id("H_e".to_string())
            .name(Some("H".to_string()))
            .compartment(Some("e".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let a_c = MetaboliteBuilder::default()
            .id("A_c".to_string())
            .name(Some("A".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let b_c = MetaboliteBuilder::default()
            .id("B_c".to_string())
            .name(Some("B".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let c_c = MetaboliteBuilder::default()
            .id("C_c".to_string())
            .name(Some("C".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let f_c = MetaboliteBuilder::default()
            .id("F_c".to_string())
            .name(Some("F".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let g_c = MetaboliteBuilder::default()
            .id("G_c".to_string())
            .name(Some("G".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let h_c = MetaboliteBuilder::default()
            .id("H_c".to_string())
            .name(Some("H".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let d_c = MetaboliteBuilder::default()
            .id("D_c".to_string())
            .name(Some("D".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        let e_c = MetaboliteBuilder::default()
            .id("E_c".to_string())
            .name(Some("E".to_string()))
            .compartment(Some("c".to_string()))
            .formula(Some("C8H18".to_string()))
            .build()
            .unwrap();
        // endregion Create Metabolites
        // region Create Genes
        let g_a_imp = GeneBuilder::default()
            .id("g_A_imp".to_string())
            .build()
            .unwrap();
        let g_b_imp = GeneBuilder::default()
            .id("g_B_imp".to_string())
            .build()
            .unwrap();
        let g_c_imp = GeneBuilder::default()
            .id("g_C_imp".to_string())
            .build()
            .unwrap();
        let g_f_exp = GeneBuilder::default()
            .id("g_F_exp".to_string())
            .build()
            .unwrap();
        let g_g_exp = GeneBuilder::default()
            .id("g_G_exp".to_string())
            .build()
            .unwrap();
        let g_h_exp = GeneBuilder::default()
            .id("g_G_exp".to_string())
            .build()
            .unwrap();
        let g_a_b_d_e = GeneBuilder::default()
            .id("g_A_B_D_E".to_string())
            .build()
            .unwrap();
        let g_c_e_f = GeneBuilder::default()
            .id("g_C_E_F".to_string())
            .build()
            .unwrap();
        let g_c_h = GeneBuilder::default()
            .id("g_C_H".to_string())
            .build()
            .unwrap();
        let g_d_g = GeneBuilder::default()
            .id("g_D_G".to_string())
            .build()
            .unwrap();
        // endregion Create Genes
        // region Create Reactions
        let r_a_e_ex = ReactionBuilder::default()
            .id("r_a_e_ex".to_string())
            .name(Some("External A exchange".to_string()))
            .lower_bound(-50f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("A_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_b_e_ex = ReactionBuilder::default()
            .id("r_b_e_ex".to_string())
            .name(Some("External B exchange".to_string()))
            .lower_bound(-50f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("B_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_c_e_ex = ReactionBuilder::default()
            .id("r_c_e_ex".to_string())
            .name(Some("External C exchange".to_string()))
            .lower_bound(-50f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("C_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_f_e_ex = ReactionBuilder::default()
            .id("r_f_e_ex".to_string())
            .name(Some("External F exchange".to_string()))
            .lower_bound(0f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("F_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_g_e_ex = ReactionBuilder::default()
            .id("r_g_e_ex".to_string())
            .name(Some("External G exchange".to_string()))
            .lower_bound(0f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("G_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_h_e_ex = ReactionBuilder::default()
            .id("r_h_e_ex".to_string())
            .name(Some("External H exchange".to_string()))
            .lower_bound(0f64)
            .upper_bound(50f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("H_e".to_string(), -1f64);
                metabolites
            })
            .subsystem(Some("Exchange".to_string()))
            .build()
            .unwrap();
        let r_a_imp = ReactionBuilder::default()
            .id("r_a_imp".to_string())
            .name(Some("A import reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("A_e".to_string(), -1f64);
                metabolites.insert("A_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("Import".to_string()))
            .gpr_string(Some("g_A_imp".to_string()))
            .build()
            .unwrap();
        let r_b_imp = ReactionBuilder::default()
            .id("r_b_imp".to_string())
            .name(Some("B import reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("B_e".to_string(), -1f64);
                metabolites.insert("B_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("Import".to_string()))
            .gpr_string(Some("g_B_imp".to_string()))
            .build()
            .unwrap();
        let r_c_imp = ReactionBuilder::default()
            .id("r_c_imp".to_string())
            .name(Some("C import reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("C_e".to_string(), -1f64);
                metabolites.insert("C_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("Import".to_string()))
            .gpr_string(Some("g_C_imp".to_string()))
            .build()
            .unwrap();
        let r_f_exp = ReactionBuilder::default()
            .id("r_f_exp".to_string())
            .name(Some("F export reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("F_c".to_string(), -1f64);
                metabolites.insert("F_e".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("Export".to_string()))
            .gpr_string(Some("g_F_exp".to_string()))
            .build()
            .unwrap();
        let r_g_exp = ReactionBuilder::default()
            .id("r_g_exp".to_string())
            .name(Some("G export reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("G_c".to_string(), -1f64);
                metabolites.insert("G_e".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("Export".to_string()))
            .gpr_string(Some("g_G_exp".to_string()))
            .build()
            .unwrap();
        let r_h_exp = ReactionBuilder::default()
            .id("r_h_exp".to_string())
            .name(Some("H export reaction".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("H_c".to_string(), -1f64);
                metabolites.insert("H_e".to_string(), 1f64);
                metabolites
            })
            .gpr_string(Some("g_H_exp".to_string()))
            .subsystem(Some("Export".to_string()))
            .build()
            .unwrap();
        let r_a_b_d_e = ReactionBuilder::default()
            .id("r_a_b_d_e".to_string())
            .name(Some("A + B -> D + E".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("A_c".to_string(), -1f64);
                metabolites.insert("B_c".to_string(), -1f64);
                metabolites.insert("D_c".to_string(), 1f64);
                metabolites.insert("E_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("internal".to_string()))
            .gpr_string(Some("g_A_B_D_E".to_string()))
            .build()
            .unwrap();
        let r_c_e_f = ReactionBuilder::default()
            .id("r_c_e_f".to_string())
            .name(Some("C -> F".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("C_c".to_string(), -1f64);
                metabolites.insert("E_c".to_string(), -1f64);
                metabolites.insert("F_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("internal".to_string()))
            .gpr_string(Some("g_C_E_F".to_string()))
            .build()
            .unwrap();
        let r_c_h = ReactionBuilder::default()
            .id("r_c_h".to_string())
            .name(Some("C -> H".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("C_c".to_string(), -1f64);
                metabolites.insert("H_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("internal".to_string()))
            .gpr_string(Some("g_C_H".to_string()))
            .build()
            .unwrap();
        let r_d_g = ReactionBuilder::default()
            .id("r_d_g".to_string())
            .name(Some("D -> G".to_string()))
            .lower_bound(-100f64)
            .upper_bound(100f64)
            .metabolites({
                let mut metabolites = IndexMap::new();
                metabolites.insert("D_c".to_string(), -1f64);
                metabolites.insert("G_c".to_string(), 1f64);
                metabolites
            })
            .subsystem(Some("internal".to_string()))
            .gpr_string(Some("g_C_H".to_string()))
            .build()
            .unwrap();
        // endregion Create Reactions
        // region Add to Model
        todo!()
        // endregion Add to Model

        //test_model
    }

    #[test]
    fn test_simple_optimization() {}
}
