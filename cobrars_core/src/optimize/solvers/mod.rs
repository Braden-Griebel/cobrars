#[cfg(feature = "scip")]
pub mod scip;

pub mod clarabel;

pub trait BasicSolver {
    
}

pub trait LinearSolver: BasicSolver {
    
}

pub trait QuadraticSolver: BasicSolver {
    
}

pub trait MixedIntegerSolver: BasicSolver {
    
}
