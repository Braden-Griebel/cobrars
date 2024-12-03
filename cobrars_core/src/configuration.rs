use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

pub static CONFIGURATION: LazyLock<Mutex<Configuration>> =
    LazyLock::new(|| Mutex::new(Configuration::default()));

pub struct Configuration {
    pub lower_bound: f64,
    pub upper_bound: f64,
    pub tolerance: f64,
    pub solver: Solver,
    pub processes: u32,
}

impl Default for Configuration {
    fn default() -> Self {
        Configuration {
            lower_bound: -1000.,
            upper_bound: 1000.,
            tolerance: 1e-07,
            solver: Solver::Clarabel,
            processes: 1,
        }
    }
}

/// Enum used to specify the default solver to use
pub enum Solver {
    /// Use the Clarabel chordal decomposition solver
    Clarabel,
    /// Use the SCIP Mixed Integer Solver
    Scip,
}
