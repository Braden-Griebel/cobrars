//! This module provides a struct for representing reactions

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