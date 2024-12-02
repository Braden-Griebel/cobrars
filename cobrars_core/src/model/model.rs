//! This module provides the Model struct for representing an entire metabolic model
use crate::model::gene::Gene;
use crate::model::metabolite::Metabolite;
use crate::model::reaction::Reaction;

use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

struct Model {
    reactions: IndexMap<String, Reaction>,
    genes: IndexMap<String, Rc<RefCell<Gene>>>,
    metabolites: IndexMap<String, Metabolite>,
    serialized_from: SerializedFrom,
}



pub(crate) enum SerializedFrom {
    None,
    Json,
    Yaml,
    Mat,
}
