//! This module provides the metabolite struct representing a metabolite

use std::hash::Hash;

/// Represents a metabolite
#[derive(Debug, Clone)]
pub struct Metabolite {
    /// Used to identify the metabolite (must be unique)
    id: String,
    /// Human Readable name of the metabolite
    name: Option<String>,
    /// Which compartment the metabolite is in
    compartment: Option<String>,
    /// Electrical charge of the Metabolite
    charge: i32,
    /// Chemical Formula of the metabolite
    formula: Option<String>,
    /// Notes about the metabolite
    notes: Option<String>,
    /// Metabolite annotations
    annotation: Option<String>,

}

impl Hash for Metabolite {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state); // Hash only depends on id
    }
}
impl Metabolite {
    /// Create a new metabolite with only an id
    pub fn new_id_only(id: String) -> Metabolite {
        Metabolite {
            id,
            formula: None,
            name: None,
            compartment: None,
            charge: 0,
            notes: None,
            annotation: None,
        }
    }

    /// Create a new metabolite
    pub fn new(
        id: String,
        formula: Option<String>,
        name: Option<String>,
        compartment: Option<String>,
        charge: i32,
        notes: Option<String>,
        annotation: Option<String>,
    ) -> Metabolite {
        Metabolite {
            id,
            formula,
            name,
            compartment,
            charge,
            notes,
            annotation,
        }
    }
}
