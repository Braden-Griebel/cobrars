//! This module provides a struct for representing reactions
use super::model::Gpr;
use crate::configuration::CONFIGURATION;
use crate::utils::hashing::hash_as_hex_string;
use derive_builder::Builder;
use indexmap::IndexMap;
use crate::metabolic_model::gene::{Gene, GeneActivity};

/// Represents a reaction in the metabolic model
#[derive(Builder, Debug, Clone)]
pub struct Reaction {
    /// Used to identify the reaction
    pub id: String,
    /// Metabolite stoichiometry of the reaction
    #[builder(default = "IndexMap::new()")]
    pub metabolites: IndexMap<String, f64>,
    /// Human-readable reaction name
    #[builder(default = "None")]
    pub name: Option<String>,
    /// Gene Protein Reaction rule to determine if reaction is active
    #[builder(default = "None")]
    pub gpr: Option<Gpr>,
    /// Lower flux bound
    #[builder(default = "CONFIGURATION.read().unwrap().lower_bound")]
    pub lower_bound: f64,
    /// Upper flux bound
    #[builder(default = "CONFIGURATION.read().unwrap().upper_bound")]
    pub upper_bound: f64,
    /// Reaction subsystem
    #[builder(default = "None")]
    pub subsystem: Option<String>,
    /// Notes about the reaction
    #[builder(default = "None")]
    pub notes: Option<String>,
    /// Reaction Annotations
    #[builder(default = "None")]
    pub annotation: Option<String>,
    /// Reaction Activity
    #[builder(default = "ReactionActivity::Active")]
    pub activity: ReactionActivity,
    /// Was the reaction activity manually set
    ///
    /// ### Note
    /// This is used for keeping track of Model updates, and enabling the rollback of changes.
    /// Basically this acts as a pin, if the reaction activity was manually set, it will keep
    /// that activity regardless of changes to the gene activity in the GPR.
    #[builder(default = "false")]
    pub(super) activity_set: bool,
}

impl Reaction {
    /// Determine the id to be associated with the forward reaction in the optimization problem
    ///
    /// # Note:
    /// The forward id is "{reaction_id}_forward"
    pub fn get_forward_id(&self) -> String {
        format!("{}_forward", &self.id)
    }

    /// Determine the id to be associated with the reverse reaction in the optimization problem
    ///
    /// # Note:
    /// The reverse id is "{reaction_id}_reverse_{hexidecimal hash of reaction_id}"
    pub fn get_reverse_id(&self) -> String {
        format!("{}_reverse_{}", &self.id, hash_as_hex_string(&self.id))
    }

    /// Determine the upper bound of the variable associated with the forward reaction
    pub(crate) fn get_forward_upper_bound(&self) -> f64 {
        match self.activity {
            ReactionActivity::Active => {
                if self.upper_bound > 0f64 {
                    self.upper_bound
                } else {
                    0f64
                }
            }
            ReactionActivity::Inactive => 0f64,
        }
    }

    /// Determine the lower bound of the variable associated with the forward reaction
    pub(crate) fn get_forward_lower_bound(&self) -> f64 {
        match self.activity {
            ReactionActivity::Active => {
                if self.lower_bound > 0f64 {
                    self.lower_bound
                } else {
                    0f64
                }
            }
            ReactionActivity::Inactive => 0f64,
        }
    }

    /// Determine the upper bound of the variable associated with the reverse reaction
    pub(crate) fn get_reverse_upper_bound(&self) -> f64 {
        match self.activity {
            ReactionActivity::Active => {
                if self.lower_bound < 0f64 {
                    -self.lower_bound
                } else {
                    0f64
                }
            }
            ReactionActivity::Inactive => 0f64,
        }
    }

    /// Determine the lower bound of the variable associated with the reverse reaction
    pub(crate) fn get_reverse_lower_bound(&self) -> f64 {
        match self.activity {
            ReactionActivity::Active => {
                if self.upper_bound < 0f64 {
                    -self.upper_bound
                } else {
                    0f64
                }
            }
            ReactionActivity::Inactive => 0f64,
        }
    }
}

/// Whether a Reaction is active or inactive
#[derive(Clone, Debug)]
pub enum ReactionActivity {
    /// The Reaction is active and can carry flux
    Active,
    /// The Reaction is inactive and can't carry flux
    Inactive,
}

impl From<GeneActivity> for ReactionActivity {
    fn from(value: GeneActivity) -> Self {
        match value {
            GeneActivity::Active => ReactionActivity::Active,
            GeneActivity::Inactive => ReactionActivity::Inactive,
        }
    }
}
