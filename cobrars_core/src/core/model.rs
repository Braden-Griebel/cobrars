//! This module provides the core Model struct for representing an entire metabolic model
use crate::core::gene::Gene;
use crate::core::reaction::Reaction;
use crate::core::metabolite::Metabolite;

use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

struct Model {
    reactions: IndexMap<String, Reaction>,
    genes: IndexMap<String, Rc<RefCell<Gene>>>,
    metabolites: IndexMap<String, Metabolite>
}