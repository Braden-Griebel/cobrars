//! This module provides the Model struct for representing an entire metabolic model
use crate::metabolic_model::gene::Gene;
use crate::metabolic_model::metabolite::Metabolite;
use crate::metabolic_model::reaction::Reaction;
use crate::optimize::problem::Problem;

use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

/// Represents a Genome Scale Metabolic Model
pub struct Model {
    /// Map of reaction ids to Reaction Objects (which are wrapped in Rc<RefCell<>>)
    pub reactions: IndexMap<String, Rc<RefCell<Reaction>>>,
    /// Map of gene ids to Gene Objects (which are wrapped in Rc<RefCell<>>)
    pub genes: IndexMap<String, Rc<RefCell<Gene>>>,
    /// Map of metabolite ids to Metabolite Objects (which are wrapped in Rc<RefCell<>>)
    pub metabolites: IndexMap<String, Rc<RefCell<Metabolite>>>,
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
    pub fn new_empty(id: String) -> Self {
        Model {
            reactions: IndexMap::new(),
            genes: IndexMap::new(),
            metabolites: IndexMap::new(),
            objective: IndexMap::new(),
            problem: None,
            id: Some(id),
            compartments: None,
            version: None,
        }
    }
}
