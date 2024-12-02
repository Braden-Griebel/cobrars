//! Module providing JSON IO for cobrars Models

use std::error::Error;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::Path;
use crate::io::IoError;

/// Represents a JSON serialized model, used for reading and writing models in json format
#[derive(Serialize, Deserialize)]
struct JsonModel {
    metabolites: Vec<JsonMetabolite>,
    reactions: Vec<JsonReaction>,
    genes: Vec<JsonGene>,
}

impl JsonModel {
    /// Read a json file into a JsonModel
    pub fn read_to_json_model<P: AsRef<Path>>(path: P) -> Result<JsonModel, IoError>{
        let json_data = match read_to_string(path){
            Ok(data) => data,
            _ => {return Err(IoError::FileNotFound)}
        };
        Ok(match serde_json::from_str(&json_data){
            Ok(model) => model,
            Err(e) => {return Err(IoError::DeserializeError)}
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
    metabolites: HashMap<String, f64>,
    lower_bound: f64,
    upper_bound: f64,
    gene_reaction_rule: String,
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

#[cfg(test)]
mod tests {
    use crate::io::json::{JsonGene, JsonMetabolite, JsonModel, JsonReaction};
    use std::collections::HashMap;
    use std::fs::read_to_string;
    use std::path::PathBuf;

    #[test]
    fn test_json_metabolite() {
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
    fn test_json_reaction() {
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
    fn test_json_gene() {
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
    fn test_json_model() {
        let this_file = file!();
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
        assert_eq!(reaction.subsystem.clone().unwrap(), "Glycolysis/Gluconeogenesis");

        // Tests for a gene
        assert_eq!(gene.id, "b1241");
        assert_eq!(gene.name.clone().unwrap(), "adhE");
    }
}
