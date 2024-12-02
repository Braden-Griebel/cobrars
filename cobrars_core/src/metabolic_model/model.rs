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
    pub(crate) reactions: IndexMap<String, Rc<RefCell<Reaction>>>,
    /// Map of gene ids to Gene Objects (which are wrapped in Rc<RefCell<>>)
    pub(crate) genes: IndexMap<String, Rc<RefCell<Gene>>>,
    /// Map of metabolite ids to Metabolite Objects (which are wrapped in Rc<RefCell<>>)
    pub(crate) metabolites: IndexMap<String, Rc<RefCell<Metabolite>>>,
    /// Map of reaction ids to objective function coefficients
    pub(crate) objective: IndexMap<String, f64>,
    /// Underlying optimization problem
    pub(crate) problem: Option<Problem>,
}
