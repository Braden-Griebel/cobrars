//! This module provides the Model struct for representing an entire metabolic model

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};

use crate::metabolic_model::gene::{Gene, GeneActivity};
use crate::metabolic_model::metabolite::Metabolite;
use crate::metabolic_model::reaction::{Reaction, ReactionActivity};
use crate::optimize::problem::{Problem, ProblemError};
use crate::optimize::solvers::{SelectedSolver, Solver};

use crate::configuration::CONFIGURATION;
use crate::optimize;
use crate::optimize::solvers::clarabel::ClarabelSolver;
use crate::optimize::variable::VariableType;
use indexmap::IndexMap;
use thiserror::Error;

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
    pub fn add_reaction(&mut self, reaction: Reaction) {
        let id = reaction.id.clone();
        self.reactions.insert(id, reaction);
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
}

/// Functions for constructing an optimization problem from a Model
impl Model {
    pub fn optimize(&mut self) -> Result<optimize::ProblemSolution, ModelError> {
        if let None = self.problem {
            self.generate_problem();
        }

        match self.problem {
            None => Err(ModelError::ProblemGenerationError),
            Some(ref mut prob) => match self.solver {
                SelectedSolver::Clarabel => {
                    let mut solver = ClarabelSolver::new();
                    Ok(prob.solve(&mut solver)?)
                }
                SelectedSolver::Scip => {
                    todo!()
                }
                SelectedSolver::Osqp => {
                    todo!()
                }
            },
        }
    }

    pub fn generate_problem(&mut self) -> Result<(), ModelError> {
        // Ensure the reaction activity matches the current state of gene activity
        // TODO: Could add a flag to the model to check if this is necessary
        self.update_reaction_activity();
        // Create a problem from the model
        self.problem = Some(self.problem_from_model()?);
        Ok(())
    }

    /// Update reaction activity based on current state of genes
    fn update_reaction_activity(&mut self) -> Result<(), ModelError> {
        let mut reaction_activity:VecDeque<ReactionActivity> = VecDeque::new();
        // Determine the activity of all the reactions
        for reaction in self.reactions.values() {
            if reaction.activity_set {
                reaction_activity.push_back(reaction.activity.clone());
                continue
            }
            match reaction.gpr {
                None => {
                    reaction_activity.push_back(ReactionActivity::Active);
                }
                Some(ref gpr) => {
                    reaction_activity.push_back(self.eval_gpr(gpr)?.into())
                }
            }
        }
        // Now actually update the activities
        for (reaction, activity) in self.reactions.values_mut().zip(reaction_activity.into_iter()){
            reaction.activity = activity;
        }
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
                        coefs.push(coef.clone());
                        // Add the reverse variable and coefficient
                        rxns.push(rxn.get_reverse_id());
                        coefs.push(-1f64 * coef.clone());
                    });
            }
        }

        // Add constraints to the problem
        for (met_id, (var_ids, var_coefs)) in metabolite_constraints.iter() {
            associated_problem.add_new_equality_constraint(
                met_id,
                // TODO: Potentially redo the underlying constraint creation to make this less obnoxious
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
            associated_problem.add_new_linear_objective_term(&fwd_id, coef.clone())?;
            associated_problem.add_new_linear_objective_term(&rev_id, -coef.clone())?;
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
    #[error("Model failed to generate optimization problem")]
    ProblemGenerationError,
    #[error("Problem accessing reaction activity: {0}")]
    GprError(#[from] GprError),
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
