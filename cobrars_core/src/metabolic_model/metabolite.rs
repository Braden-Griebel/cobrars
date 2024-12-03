//! This module provides the metabolite struct representing a metabolite

use std::hash::Hash;

use derive_builder::Builder;

/// Represents a metabolite
#[derive(Builder, Debug, Clone)]
pub struct Metabolite {
    /// Used to identify the metabolite (must be unique)
    pub id: String,
    /// Human Readable name of the metabolite
    #[builder(default = "None")]
    pub name: Option<String>,
    /// Which compartment the metabolite is in
    #[builder(default = "None")]
    pub compartment: Option<String>,
    /// Electrical charge of the Metabolite
    #[builder(default = "0")]
    pub charge: i32,
    /// Chemical Formula of the metabolite
    #[builder(default = "None")]
    pub formula: Option<String>,
    /// Notes about the metabolite
    #[builder(default = "None")]
    pub notes: Option<String>,
    /// Metabolite annotations
    #[builder(default = "None")]
    pub annotation: Option<String>,
}

impl Hash for Metabolite {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state); // Hash by id
                             // If the metabolite has an associated compartment, also hash by that
        if let Some(ref compartment) = self.compartment {
            compartment.hash(state)
        };
    }
}
