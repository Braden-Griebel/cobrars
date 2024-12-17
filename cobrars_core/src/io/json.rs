//! Module providing JSON IO for cobrars Models
use std::cell::RefCell;
use std::fs;
use std::path::Path;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;
use crate::configuration::CONFIGURATION;
use crate::io::gpr_parse::{parse_gpr, GprParseError};
use crate::io::IoError;
use crate::metabolic_model::gene::{Gene, GeneActivity};
use crate::metabolic_model::metabolite::Metabolite;
use crate::metabolic_model::model::Model;
use crate::metabolic_model::reaction::{Reaction, ReactionBuilder, ReactionBuilderError};
use crate::optimize::solvers::Solver;

// region JSON Model
/// Represents a JSON serialized model, used for reading and writing models in json format
#[derive(Serialize, Deserialize)]
struct JsonModel {
    metabolites: Vec<JsonMetabolite>,
    reactions: Vec<JsonReaction>,
    genes: Vec<JsonGene>,
    id: Option<String>,
    compartments: Option<IndexMap<String, String>>,
    version: Option<String>,
}

impl JsonModel {
    /// Read a json file into a JsonModel
    pub fn read_to_json_model<P: AsRef<Path>>(path: P) -> Result<JsonModel, IoError> {
        let json_data = match fs::read_to_string(path) {
            Ok(data) => data,
            _ => return Err(IoError::FileNotFound),
        };
        Ok(match serde_json::from_str(&json_data) {
            Ok(model) => model,
            Err(e) => return Err(IoError::DeserializeError),
        })
    }
}

#[derive(Serialize, Deserialize)]
struct JsonMetabolite {
    id: String,
    name: Option<String>,
    compartment: Option<String>,
    charge: Option<i32>,
    formula: Option<String>,
    notes: Option<Value>,
    annotation: Option<Value>,
}

#[derive(Serialize, Deserialize)]
struct JsonReaction {
    id: String,
    name: Option<String>,
    metabolites: IndexMap<String, f64>,
    lower_bound: f64,
    upper_bound: f64,
    gene_reaction_rule: String,
    objective_coefficient: Option<f64>,
    subsystem: Option<String>,
    notes: Option<Value>,
    annotation: Option<Value>,
}

#[derive(Serialize, Deserialize)]
struct JsonGene {
    id: String,
    name: Option<String>,
    notes: Option<Value>,
    annotation: Option<Value>,
}
// endregion JSON Model

// region Conversions
impl From<JsonGene> for Gene {
    fn from(g: JsonGene) -> Self {
        /* For now this just converts the notes and annotations into a JSON string,
        which isn't ideal, but the data isn't very structured, so it would require a lot of
        maintenance to unpack more than this. This needs to be revisited when additional
        model file types are supported.
        TODO: Update this to be compatible with other file types
        (Potentially, the other file types versions of this could just be converted into
        json structured data as well and still stored as stings like this)
        */
        Self {
            id: g.id,
            name: g.name,
            activity: GeneActivity::Active, // All genes start as active
            notes: g.notes.map(|v| v.to_string()),
            annotation: g.annotation.map(|v| v.to_string()),
        }
    }
}

impl From<JsonMetabolite> for Metabolite {
    fn from(m: JsonMetabolite) -> Self {
        Self {
            id: m.id,
            name: m.name,
            compartment: m.compartment,
            charge: m.charge.unwrap_or_default(),
            formula: m.formula,
            notes: m.notes.map(|v| v.to_string()),
            annotation: m.annotation.map(|v| v.to_string()),
        }
    }
}

impl From<Gene> for JsonGene {
    fn from(g: Gene) -> Self {
        Self {
            id: g.id,
            name: g.name,
            notes: g
                .notes
                .map(|n| serde_json::from_str(&n).unwrap_or(Value::String(String::new()))),
            annotation: g
                .annotation
                .map(|a| serde_json::from_str(&a).unwrap_or(Value::String(String::new()))),
        }
    }
}

impl From<Metabolite> for JsonMetabolite {
    fn from(m: Metabolite) -> Self {
        Self {
            id: m.id,
            name: m.name,
            compartment: m.compartment,
            charge: Some(m.charge),
            formula: m.formula,
            notes: m
                .notes
                .map(|n| serde_json::from_str(&n).unwrap_or(Value::String(String::new()))),
            annotation: m
                .annotation
                .map(|a| serde_json::from_str(&a).unwrap_or(Value::String(String::new()))),
        }
    }
}

impl Model {
    pub fn read_json<P: AsRef<Path>>(path: P) -> Result<Model, JsonError> {
        let model_str = match fs::read_to_string(path) {
            Ok(data) => data,
            Err(err) => return Err(JsonError::UnableToRead(format!("{:?}", err))),
        };
        let json_model = match serde_json::from_str::<JsonModel>(&model_str) {
            Ok(model) => model,
            Err(err) => return Err(JsonError::UnableToParse(format!("{:?}", err))),
        };
        Model::from_json(json_model)
    }

    pub fn write_json<P: AsRef<Path>>(&self, path: P) -> Result<(), JsonError> {
        let json_model = self.to_json()?;
        let model_string = serde_json::to_string(&json_model)?;
        fs::write(path, model_string)?;
        Ok(())
    }

    fn from_json(json_model: JsonModel) -> Result<Self, JsonError> {
        let mut reactions: IndexMap<String, Reaction> = IndexMap::new();
        let mut genes: IndexMap<String, Gene> = IndexMap::new();
        let mut metabolites: IndexMap<String, Metabolite> = IndexMap::new();
        let mut objective: IndexMap<String, f64> = IndexMap::new();
        // Start by converting the genes and metabolites using the From methods
        json_model.genes.into_iter().for_each(|g| {
            genes.insert(g.id.clone(), Gene::from(g));
        });
        json_model.metabolites.into_iter().for_each(|m| {
            metabolites.insert(m.id.clone(), Metabolite::from(m));
        });
        /* Now, iterate through the reactions, parsing GPRs, and adding to
        the objective along the way
        */
        for rxn in json_model.reactions {
            let gpr = if !rxn.gene_reaction_rule.is_empty() {
                Some(parse_gpr(&rxn.gene_reaction_rule, &mut genes)?)
            } else {
                None
            };
            let new_reaction = ReactionBuilder::default()
                .id(rxn.id.clone())
                .metabolites(rxn.metabolites)
                .name(rxn.name)
                .gpr(gpr)
                .lower_bound(rxn.lower_bound)
                .upper_bound(rxn.upper_bound)
                .subsystem(rxn.subsystem)
                .notes(rxn.notes.map(|v| v.to_string()))
                .annotation(rxn.annotation.map(|v| v.to_string()))
                .build()?;
            reactions.insert(rxn.id.clone(), new_reaction);
            // Add the reaction to the objective function if desired
            if let Some(coef) = rxn.objective_coefficient {
                objective.insert(rxn.id, coef);
            }
        }
        let solver = CONFIGURATION.read().unwrap().solver.clone();
        Ok(Model {
            reactions,
            genes,
            metabolites,
            objective,
            problem: None,
            id: json_model.id,
            compartments: json_model.compartments,
            version: json_model.version,
            solver,
            reaction_activity_update_required: false,
        })
    }
    fn to_json(&self) -> Result<JsonModel, JsonError> {
        let json_genes: Vec<JsonGene> = self.genes.iter().map(|(_, g)| g.clone().into()).collect();
        let json_metabolites: Vec<JsonMetabolite> = self
            .metabolites
            .iter()
            .map(|(_, m)| m.clone().into())
            .collect();
        let json_id = self.id.clone().unwrap_or_default();
        let json_compartments = self.compartments.clone();
        let json_version = self.version.clone().unwrap_or_default();
        let mut json_reactions: Vec<JsonReaction> = Vec::new();
        for (_, r) in &self.reactions {
            json_reactions.push(JsonReaction {
                id: r.id.clone(),
                name: r.name.clone(),
                metabolites: Default::default(),
                lower_bound: r.lower_bound,
                upper_bound: r.upper_bound,
                gene_reaction_rule: r
                    .gpr
                    .clone()
                    .map(|rule| rule.to_string_id())
                    .unwrap_or(String::new()),
                objective_coefficient: self.objective.get(&r.id).copied(),
                subsystem: r.subsystem.clone(),
                notes: r
                    .notes
                    .clone()
                    .map(|n| serde_json::from_str(&n).unwrap_or(Value::String(n))),
                annotation: r
                    .annotation
                    .clone()
                    .map(|a| serde_json::from_str(&a).unwrap_or(Value::String(a))),
            })
        }

        Ok(JsonModel {
            metabolites: json_metabolites,
            reactions: json_reactions,
            genes: json_genes,
            id: None,
            compartments: None,
            version: None,
        })
    }
}

#[derive(Error, Debug)]
pub enum JsonError {
    #[error("Unable to parse a GPR rule during conversion from JSON")]
    GprParserError(#[from] GprParseError),
    #[error("Unable to read file due to {0}")]
    UnableToRead(String),
    #[error("Unable to parse json due to {0}")]
    UnableToParse(String),
    #[error("Unable to build reaction")]
    UnableToBuildReaction(#[from] ReactionBuilderError),
    #[error("Serde json parse error")]
    SerdeJsonParseError(#[from] serde_json::Error),
    #[error("Unable to write to file")]
    UnableToWrite(#[from] std::io::Error),
}

// endregion Conversions

#[cfg(test)]
mod json_tests {
    use crate::io::json::{JsonGene, JsonMetabolite, JsonModel, JsonReaction};
    use std::collections::HashMap;
    use std::fs::read_to_string;
    use std::path::PathBuf;

    #[test]
    fn json_metabolite() {
        let data = r#"{
"id":"glc__D_e",
"name":"D-Glucose",
"compartment":"e",
"charge":0,
"formula":"C6H12O6",
"notes":{
"original_bigg_ids":[
"glc_D_e"
]
},
"annotation":{
"bigg.metabolite":[
"glc__D"
],
"biocyc":[
"META:Glucopyranose"
],
"chebi":[
"CHEBI:12965",
"CHEBI:20999",
"CHEBI:4167",
"CHEBI:17634"
],
"hmdb":[
"HMDB00122",
"HMDB06564"
],
"inchi_key":[
"WQZGKKKJIJFFOK-GASJEMHNSA-N"
],
"kegg.compound":[
"C00031"
],
"kegg.drug":[
"D00009"
],
"metanetx.chemical":[
"MNXM41"
],
"sabiork":[
"1406",
"1407"
],
"sbo":"SBO:0000247",
"seed.compound":[
"cpd26821",
"cpd00027"
]
}
}"#;
        let met: JsonMetabolite = serde_json::from_str(data).unwrap();
        assert_eq!(met.id, "glc__D_e");
        assert_eq!(met.name.unwrap(), "D-Glucose");
        assert_eq!(met.compartment.unwrap(), "e");
        assert_eq!(met.charge.unwrap(), 0);
        assert_eq!(met.formula.unwrap(), "C6H12O6");
    }

    #[test]
    fn json_reaction() {
        let data = r#"{
"id":"PFK",
"name":"Phosphofructokinase",
"metabolites":{
"adp_c":1.0,
"atp_c":-1.0,
"f6p_c":-1.0,
"fdp_c":1.0,
"h_c":1.0
},
"lower_bound":0.0,
"upper_bound":1000.0,
"gene_reaction_rule":"b3916 or b1723",
"subsystem":"Glycolysis/Gluconeogenesis",
"notes":{
"original_bigg_ids":[
"PFK"
]
},
"annotation":{
"bigg.reaction":[
"PFK"
],
"ec-code":[
"2.7.1.11"
],
"metanetx.reaction":[
"MNXR102507"
],
"rhea":[
"16111",
"16109",
"16110",
"16112"
],
"sbo":"SBO:0000176"
}
}"#;
        let reaction: JsonReaction = serde_json::from_str(data).unwrap();
        assert_eq!(reaction.id, "PFK");
        assert_eq!(reaction.name.unwrap(), "Phosphofructokinase");
        let mut expected_reactions: HashMap<String, f64> = HashMap::new();
        expected_reactions.insert("adp_c".to_string(), 1.0);
        expected_reactions.insert("atp_c".to_string(), -1.0);
        expected_reactions.insert("f6p_c".to_string(), -1.0);
        expected_reactions.insert("fdp_c".to_string(), 1.0);
        expected_reactions.insert("h_c".to_string(), 1.0);
        for (k, v) in reaction.metabolites {
            assert!((v - expected_reactions.get(&k).unwrap()).abs() < 1e-25);
        }
        assert!((reaction.lower_bound - 0.0).abs() < 1e-25);
        assert!((reaction.upper_bound - 1000.0).abs() < 1e-25);
        assert_eq!(reaction.gene_reaction_rule, "b3916 or b1723");
        assert_eq!(reaction.subsystem.unwrap(), "Glycolysis/Gluconeogenesis");
    }

    #[test]
    fn json_gene() {
        let data = r#"
        {
"id":"b1241",
"name":"adhE",
"notes":{
"original_bigg_ids":[
"b1241"
]
},
"annotation":{
"asap":[
"ABE-0004164"
],
"ecogene":[
"EG10031"
],
"ncbigene":[
"945837"
],
"ncbigi":[
"16129202"
],
"refseq_locus_tag":[
"b1241"
],
"refseq_name":[
"adhE"
],
"refseq_synonym":[
"JW1228",
"ECK1235",
"adhC",
"ana"
],
"sbo":"SBO:0000243",
"uniprot":[
"P0A9Q7"
]
}
}
        "#;
        let gene: JsonGene = serde_json::from_str(data).unwrap();
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.unwrap(), "adhE");
    }

    #[test]
    fn json_model() {
        let data_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_data")
            .join("test_models")
            .join("e_coli_core.json");
        let ecoli_model = read_to_string(data_path).unwrap();
        let model: JsonModel = serde_json::from_str(&ecoli_model).unwrap();
        let met = model.metabolites.first().unwrap();
        let reaction = model.reactions.first().unwrap();
        let gene = model.genes.first().unwrap();

        // Tests for the metabolite
        assert_eq!(met.id, "glc__D_e");
        assert_eq!(met.name.clone().unwrap(), "D-Glucose");
        assert_eq!(met.compartment.clone().unwrap(), "e");
        assert_eq!(met.charge.unwrap(), 0);
        assert_eq!(met.formula.clone().unwrap(), "C6H12O6");

        // Tests for the reaction
        assert_eq!(reaction.id, "PFK");
        assert_eq!(reaction.name.clone().unwrap(), "Phosphofructokinase");
        let mut expected_reactions: HashMap<String, f64> = HashMap::new();
        expected_reactions.insert("adp_c".to_string(), 1.0);
        expected_reactions.insert("atp_c".to_string(), -1.0);
        expected_reactions.insert("f6p_c".to_string(), -1.0);
        expected_reactions.insert("fdp_c".to_string(), 1.0);
        expected_reactions.insert("h_c".to_string(), 1.0);
        for (k, v) in &reaction.metabolites {
            assert!((v - expected_reactions.get(k).unwrap()).abs() < 1e-25);
        }
        assert!((reaction.lower_bound - 0.0).abs() < 1e-25);
        assert!((reaction.upper_bound - 1000.0).abs() < 1e-25);
        assert_eq!(reaction.gene_reaction_rule, "b3916 or b1723");
        assert_eq!(
            reaction.subsystem.clone().unwrap(),
            "Glycolysis/Gluconeogenesis"
        );

        // Tests for a gene
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.clone().unwrap(), "adhE");
    }
}

#[cfg(test)]
mod model_tests {
    use super::*;
    use crate::metabolic_model::model::{Gpr, GprOperation};
    use crate::optimize::solvers::clarabel::ClarabelSolver;
    use std::collections::HashMap;
    use std::path::PathBuf;

    #[test]
    fn gene_conversion() {
        let data = r#"
        {
"id":"b1241",
"name":"adhE",
"notes":{
"original_bigg_ids":[
"b1241"
]
},
"annotation":{
"asap":[
"ABE-0004164"
],
"ecogene":[
"EG10031"
],
"ncbigene":[
"945837"
],
"ncbigi":[
"16129202"
],
"refseq_locus_tag":[
"b1241"
],
"refseq_name":[
"adhE"
],
"refseq_synonym":[
"JW1228",
"ECK1235",
"adhC",
"ana"
],
"sbo":"SBO:0000243",
"uniprot":[
"P0A9Q7"
]
}
}
        "#;
        let json_gene: JsonGene = serde_json::from_str(data).unwrap();
        let model_gene = Gene::from(json_gene);
        assert_eq!(model_gene.id, "b1241");
        assert_eq!(model_gene.name.unwrap(), "adhE");
    }

    #[test]
    fn metabolite() {
        let data = r#"{
"id":"glc__D_e",
"name":"D-Glucose",
"compartment":"e",
"charge":0,
"formula":"C6H12O6",
"notes":{
"original_bigg_ids":[
"glc_D_e"
]
},
"annotation":{
"bigg.metabolite":[
"glc__D"
],
"biocyc":[
"META:Glucopyranose"
],
"chebi":[
"CHEBI:12965",
"CHEBI:20999",
"CHEBI:4167",
"CHEBI:17634"
],
"hmdb":[
"HMDB00122",
"HMDB06564"
],
"inchi_key":[
"WQZGKKKJIJFFOK-GASJEMHNSA-N"
],
"kegg.compound":[
"C00031"
],
"kegg.drug":[
"D00009"
],
"metanetx.chemical":[
"MNXM41"
],
"sabiork":[
"1406",
"1407"
],
"sbo":"SBO:0000247",
"seed.compound":[
"cpd26821",
"cpd00027"
]
}
}"#;
        let met: JsonMetabolite = serde_json::from_str(data).unwrap();
        let model_met = Metabolite::from(met);
        assert_eq!(model_met.id, "glc__D_e");
        assert_eq!(model_met.name.unwrap(), "D-Glucose");
        assert_eq!(model_met.compartment.unwrap(), "e");
        assert_eq!(model_met.charge, 0);
        assert_eq!(model_met.formula.unwrap(), "C6H12O6");
    }

    #[test]
    fn json_conversion() {
        let this_file = file!();
        let data_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_data")
            .join("test_models")
            .join("e_coli_core.json");
        let ecoli_model = fs::read_to_string(data_path).unwrap();
        let model: JsonModel = serde_json::from_str(&ecoli_model).unwrap();

        let metabolic_model: Model = Model::from_json(model).unwrap();
        let (_, met) = metabolic_model.metabolites.first().unwrap();
        let (_, reaction) = metabolic_model.reactions.first().unwrap();
        let (_, gene) = metabolic_model.genes.first().unwrap();

        // Tests for the metabolite
        assert_eq!(met.id, "glc__D_e");
        assert_eq!(met.name.clone().unwrap(), "D-Glucose");
        assert_eq!(met.compartment.clone().unwrap(), "e");
        assert_eq!(met.charge, 0);
        assert_eq!(met.formula.clone().unwrap(), "C6H12O6");

        // Tests for the reaction
        assert_eq!(reaction.id, "PFK");
        assert_eq!(reaction.name.clone().unwrap(), "Phosphofructokinase");
        let mut expected_reactions: IndexMap<String, f64> = IndexMap::new();
        expected_reactions.insert("adp_c".to_string(), 1.0);
        expected_reactions.insert("atp_c".to_string(), -1.0);
        expected_reactions.insert("f6p_c".to_string(), -1.0);
        expected_reactions.insert("fdp_c".to_string(), 1.0);
        expected_reactions.insert("h_c".to_string(), 1.0);
        for (k, v) in &reaction.metabolites {
            assert!((v - expected_reactions.get(k).unwrap()).abs() < 1e-25);
        }
        assert!((reaction.lower_bound - 0.0).abs() < 1e-25);
        assert!((reaction.upper_bound - 1000.0).abs() < 1e-25);
        assert_eq!(
            reaction.subsystem.clone().unwrap(),
            "Glycolysis/Gluconeogenesis"
        );

        match reaction.gpr {
            None => {}
            Some(ref rule) => match rule {
                Gpr::Operation(op) => match op {
                    GprOperation::Or { left, right } => {
                        match (**left).clone() {
                            Gpr::GeneNode(g) => {
                                if g != "b3916" {
                                    panic!("Incorrect Parse")
                                }
                            }
                            _ => panic!("Incorrect Parse"),
                        };
                        match (**right).clone() {
                            Gpr::GeneNode(g) => {
                                if g != "b1723" {
                                    panic!("Incorrect Parse")
                                }
                            }
                            _ => panic!("Incorrect Parse"),
                        };
                    }
                    _ => {
                        panic!("Incorrect Parse")
                    }
                },
                Gpr::GeneNode(_) => {
                    panic!("Incorrect Parse")
                }
            },
        }

        // Tests for a gene
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.clone().unwrap(), "adhE");
    }

    #[test]
    fn read_json() {
        let data_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_data")
            .join("test_models")
            .join("e_coli_core.json");
        let model: Model = Model::read_json(data_path).unwrap();
        let (_, met) = model.metabolites.first().unwrap();
        let (_, reaction) = model.reactions.first().unwrap();
        let (_, gene) = model.genes.first().unwrap();

        // Tests for the metabolite
        assert_eq!(met.id, "glc__D_e");
        assert_eq!(met.name.clone().unwrap(), "D-Glucose");
        assert_eq!(met.compartment.clone().unwrap(), "e");
        assert_eq!(met.charge, 0);
        assert_eq!(met.formula.clone().unwrap(), "C6H12O6");

        // Tests for the reaction
        assert_eq!(reaction.id, "PFK");
        assert_eq!(reaction.name.clone().unwrap(), "Phosphofructokinase");
        let mut expected_reactions: IndexMap<String, f64> = IndexMap::new();
        expected_reactions.insert("adp_c".to_string(), 1.0);
        expected_reactions.insert("atp_c".to_string(), -1.0);
        expected_reactions.insert("f6p_c".to_string(), -1.0);
        expected_reactions.insert("fdp_c".to_string(), 1.0);
        expected_reactions.insert("h_c".to_string(), 1.0);
        for (k, v) in &reaction.metabolites {
            assert!((v - expected_reactions.get(k).unwrap()).abs() < 1e-25);
        }
        assert!((reaction.lower_bound - 0.0).abs() < 1e-25);
        assert!((reaction.upper_bound - 1000.0).abs() < 1e-25);
        assert_eq!(
            reaction.subsystem.clone().unwrap(),
            "Glycolysis/Gluconeogenesis"
        );

        match reaction.gpr {
            None => {}
            Some(ref rule) => match rule {
                Gpr::Operation(op) => match op {
                    GprOperation::Or { left, right } => {
                        match (**left).clone() {
                            Gpr::GeneNode(g) => {
                                if g != "b3916" {
                                    panic!("Incorrect Parse")
                                }
                            }
                            _ => panic!("Incorrect Parse"),
                        };
                        match (**right).clone() {
                            Gpr::GeneNode(g) => {
                                if g != "b1723" {
                                    panic!("Incorrect Parse")
                                }
                            }
                            _ => panic!("Incorrect Parse"),
                        };
                    }
                    _ => {
                        panic!("Incorrect Parse")
                    }
                },
                Gpr::GeneNode(_) => {
                    panic!("Incorrect Parse")
                }
            },
        }

        // Tests for a gene
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.clone().unwrap(), "adhE");

        // Tests for id
        assert_eq!(model.id.clone().unwrap(), "e_coli_core");

        // Tests for version
        assert_eq!(model.version.clone().unwrap(), "1");

        // Tests for compartments
        let mut expected_compartments: IndexMap<String, String> = IndexMap::new();
        expected_compartments.insert("c".to_string(), "cytosol".to_string());
        expected_compartments.insert("e".to_string(), "extracellular space".to_string());
        assert_eq!(model.compartments.clone().unwrap(), expected_compartments);
    }

    #[test]
    fn to_json() {
        // Read in the JSON model
        let data_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_data")
            .join("test_models")
            .join("e_coli_core.json");
        let model: Model = Model::read_json(data_path).unwrap();

        // Convert the model to a json string
        let json_model = model.to_json().unwrap();

        // Check on the first metabolite, reaction, and gene
        let met = json_model.metabolites.first().unwrap();
        let reaction = json_model.reactions.first().unwrap();
        let gene = json_model.genes.first().unwrap();

        // Metabolite tests
        assert_eq!(met.id, "glc__D_e");
        assert_eq!(met.name.clone().unwrap(), "D-Glucose");
        assert_eq!(met.compartment.clone().unwrap(), "e");
        assert_eq!(met.charge.unwrap(), 0);
        assert_eq!(met.formula.clone().unwrap(), "C6H12O6");

        // Reaction tests
        assert_eq!(reaction.id, "PFK");
        assert_eq!(reaction.name.clone().unwrap(), "Phosphofructokinase");
        let mut expected_reactions: HashMap<String, f64> = HashMap::new();
        expected_reactions.insert("adp_c".to_string(), 1.0);
        expected_reactions.insert("atp_c".to_string(), -1.0);
        expected_reactions.insert("f6p_c".to_string(), -1.0);
        expected_reactions.insert("fdp_c".to_string(), 1.0);
        expected_reactions.insert("h_c".to_string(), 1.0);
        for (k, v) in &reaction.metabolites {
            assert!((v - expected_reactions.get(k).unwrap()).abs() < 1e-25);
        }
        assert!((reaction.lower_bound - 0.0).abs() < 1e-25);
        assert!((reaction.upper_bound - 1000.0).abs() < 1e-25);
        assert_eq!(reaction.gene_reaction_rule, "(b3916 or b1723)");
        assert_eq!(
            reaction.subsystem.clone().unwrap(),
            "Glycolysis/Gluconeogenesis"
        );

        // Tests for a gene
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.clone().unwrap(), "adhE");
    }
}
