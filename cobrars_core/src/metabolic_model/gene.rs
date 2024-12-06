//! This module provides the Gene struct, representing a gene, and the GPR struct, representing a
//! gene protein reaction rule
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::sync::{Arc, RwLock};

use derive_builder::Builder;

/// Structure Representing a Gene
#[derive(Builder, Clone, Debug, Eq, PartialEq)]
pub struct Gene {
    /// Used to identify the gene
    pub id: String,
    /// Human Readable Gene Name
    #[builder(default = "None")]
    pub name: Option<String>,
    /// Whether this gene is currently active (see [`GeneActivity`])
    #[builder(default = "GeneActivity::Active")]
    pub activity: GeneActivity,
    /// Notes about the gene
    #[builder(default = "None")]
    pub notes: Option<String>,
    /// Gene Annotations
    #[builder(default = "None")]
    pub annotation: Option<String>,
}

impl Gene {
    pub fn new(
        id: String,
        name: Option<String>,
        activity: GeneActivity,
        notes: Option<String>,
        annotation: Option<String>,
    ) -> Gene {
        GeneBuilder::default()
            .id(id)
            .name(name)
            .activity(activity)
            .notes(notes)
            .annotation(annotation)
            .build()
            .unwrap()
    }
}

impl Display for Gene {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

/// Whether a gene is active or not
#[derive(Clone, Debug, Hash, Eq, PartialEq, Copy)]
pub enum GeneActivity {
    /// Gene is considered active
    Active,
    /// Gene is considered inactive
    Inactive,
}
