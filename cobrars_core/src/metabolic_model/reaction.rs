//! This module provides a struct for representing reactions
use derive_builder::Builder;
use super::gene::Gpr;
use crate::utils::hashing::hash_as_hex_string;
use crate::configuration::CONFIGURATION;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

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
    #[builder(default = "CONFIGURATION.lock().unwrap().lower_bound")]
    pub lower_bound: f64,
    /// Upper flux bound
    #[builder(default = "CONFIGURATION.lock().unwrap().upper_bound")]
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
}
