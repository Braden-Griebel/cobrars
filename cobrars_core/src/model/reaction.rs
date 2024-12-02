//! This module provides a struct for representing reactions
use crate::utils::hashing::hash_as_hex_string;
use super::gene::Gpr;

/// Represents a reaction in the metabolic model
pub struct Reaction {
    /// Used to identify the reaction
    id: String,
    /// Human-readable reaction name
    name: String,
    /// Gene Protein Reaction rule to determine if reaction is active
    gpr: Option<Gpr>,
    /// Lower flux bound
    lower_bound: f64,
    /// Upper flux bound
    upper_bound: f64,
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