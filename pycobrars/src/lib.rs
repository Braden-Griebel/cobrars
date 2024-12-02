pub mod core;

use cobrars_core;

use pyo3::prelude::*;

#[pyclass]
struct PyGene {
    inner: cobrars_core::model::gene::Gene,
}

#[pymethods]
impl PyGene {
    #[new]
    fn new(id: String) -> Self {
        let inner_gene = cobrars_core::model::gene::Gene::new(
            id,
            None,
            cobrars_core::model::gene::GeneActivity::Active,
        );
        PyGene { inner: inner_gene }
    }

    fn get_name(&self) -> String {
        self.inner.id.clone()
    }
}

#[pyfunction]
fn hello_from_bin() -> String {
    "Hello from pycobrars!".to_string()
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn _core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(hello_from_bin, m)?)?;
    m.add_class::<PyGene>()?;
    Ok(())
}
