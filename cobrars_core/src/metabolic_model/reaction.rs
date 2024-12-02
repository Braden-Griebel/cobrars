//! This module provides a struct for representing reactions
use super::gene::Gpr;
use crate::utils::hashing::hash_as_hex_string;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

/// Represents a reaction in the metabolic model
pub struct Reaction {
    /// Used to identify the reaction
    pub id: String,
    /// Metabolite stoichiometry of the reaction
    pub metabolites: IndexMap<String, f64>,
    /// Human-readable reaction name
    pub name: Option<String>,
    /// Gene Protein Reaction rule to determine if reaction is active
    pub gpr: Option<Gpr>,
    /// Lower flux bound
    pub lower_bound: f64,
    /// Upper flux bound
    pub upper_bound: f64,
    /// Reaction subsystem
    pub subsystem: Option<String>,
    /// Notes about the reaction
    pub notes: Option<String>,
    /// Reaction Annotations
    pub annotation: Option<String>,
}

impl Reaction {
    /// Create a new reaction wrapped in a reference
    ///
    /// # Returns
    /// A new reaction wrapped in a Rc<RefCell<>>
    pub fn new_wrapped(
        id: String,
        metabolites: IndexMap<String, f64>,
        name: Option<String>,
        gpr: Option<Gpr>,
        lower_bound: f64,
        upper_bound: f64,
        subsystem: Option<String>,
        notes: Option<String>,
        annotation: Option<String>,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            id,
            metabolites,
            name,
            gpr,
            lower_bound,
            upper_bound,
            subsystem,
            notes,
            annotation,
        }))
    }
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
