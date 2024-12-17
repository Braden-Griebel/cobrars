use std::sync::{LazyLock, RwLock};
use crate::optimize::solvers::{SelectedSolver, Solver};

pub static CONFIGURATION: LazyLock<RwLock<Configuration>> =
    LazyLock::new(|| RwLock::new(Configuration::default()));

pub struct Configuration {
    pub lower_bound: f64,
    pub upper_bound: f64,
    pub tolerance: f64,
    pub solver: SelectedSolver,
    pub processes: u32,
}

impl Default for Configuration {
    fn default() -> Self {
        Configuration {
            lower_bound: -1000.,
            upper_bound: 1000.,
            tolerance: 1e-07,
            solver: SelectedSolver::Clarabel,
            processes: 1,
        }
    }
}

