//! This module provides the Model struct for representing an entire metabolic model
use std::fmt::{Display, Formatter};

use crate::metabolic_model::gene::{Gene, GeneActivity};
use crate::metabolic_model::metabolite::Metabolite;
use crate::metabolic_model::reaction::Reaction;
use crate::optimize::problem::Problem;
use crate::optimize::solvers::Solver;

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
    /// Id associated with the Model
    pub id: Option<String>,
    /// Compartments in the model
    ///
    /// An IndexMap<String, String> of {short name: long name}
    pub compartments: Option<IndexMap<String, String>>,
    /// A version identifier for the Model, stored as a string
    pub version: Option<String>,
}

impl Model {
    pub fn new_empty() -> Self {
        Model {
            reactions: IndexMap::new(),
            genes: IndexMap::new(),
            metabolites: IndexMap::new(),
            objective: IndexMap::new(),
            problem: None,
            id: None,
            compartments: None,
            version: None,
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
    #[error("Inavlid Binary Operation")]
    InvalidBinaryOp,
    #[error("Inavlid Unary Operation")]
    InvalidUnaryOp,
    #[error("Gene is GPR is not present in the model")]
    GeneNotFound,
}

// Model associated frunctions for working with GPRs
impl Model {
    /// Evaluate whether a GPR evaluates to Active or Inactive
    pub fn eval_gpr(&self, gpr: Gpr) -> Result<GeneActivity, GprError> {
        match gpr {
            Gpr::Operation(op) => match op {
                GprOperation::Or { left, right } => {
                    let l = self.eval_gpr(*left)?;
                    let r = self.eval_gpr(*right)?;
                    if l == GeneActivity::Active || r == GeneActivity::Active {
                        Ok(GeneActivity::Active)
                    } else {
                        Ok(GeneActivity::Inactive)
                    }
                }
                GprOperation::And { left, right } => {
                    let l = self.eval_gpr(*left)?;
                    let r = self.eval_gpr(*right)?;
                    if l == GeneActivity::Active && r == GeneActivity::Active {
                        Ok(GeneActivity::Active)
                    } else {
                        Ok(GeneActivity::Inactive)
                    }
                }
                GprOperation::Not { val } => match self.eval_gpr(*val)? {
                    GeneActivity::Active => Ok(GeneActivity::Inactive),
                    GeneActivity::Inactive => Ok(GeneActivity::Active),
                },
            },
            Gpr::GeneNode(gene) => match self.genes.get(&gene) {
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

        return model;
    }

    #[test]
    fn gene_node() {
        let model = setup_model();
        let active_gene_node = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node = Gpr::GeneNode("inactive_gene1".to_string());
        assert_eq!(
            model.eval_gpr(active_gene_node).unwrap(),
            GeneActivity::Active
        );
        assert_eq!(
            model.eval_gpr(inactive_gene_node).unwrap(),
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
            model.eval_gpr(gpr_and_active).unwrap(),
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
            model.eval_gpr(gpr_and_inactive).unwrap(),
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
            model.eval_gpr(gpr_and_inactive).unwrap(),
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

        assert_eq!(model.eval_gpr(gpr_or_active).unwrap(), GeneActivity::Active);

        // With an active or an inactive gene
        let active_gene_node = Gpr::GeneNode("active_gene1".to_string());
        let inactive_gene_node = Gpr::GeneNode("inactive_gene1".to_string());
        let gpr_or_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene_node.clone()),
            right: Box::new(inactive_gene_node.clone()),
        });

        assert_eq!(
            model.eval_gpr(gpr_or_inactive).unwrap(),
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
            model.eval_gpr(gpr_or_inactive).unwrap(),
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
            model.eval_gpr(gpr_not_inactive).unwrap(),
            GeneActivity::Inactive
        );

        // Next check the inactive
        let gpr_not_active = Gpr::Operation(GprOperation::Not {
            val: Box::new(inactive_gene_node1),
        });
        assert_eq!(
            model.eval_gpr(gpr_not_active).unwrap(),
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
