//! This module provides the Gene struct, representing a gene, and the GPR struct, representing a
//! gene protein reaction rule
use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;
use std::borrow::Borrow;

/// Structure Representing a Gene
#[derive(Clone, Debug)]
pub struct Gene {
    /// Used to identify the gene
    id: String,
    /// Human Readable Gene Name
    name: Option<String>,
    /// Whether this gene is currently active (see [`GeneActivity`])
    activity: GeneActivity,
}

impl Hash for Gene {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.activity.hash(state);
    }
}


/// Whether a gene is active or not
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum GeneActivity {
    /// Gene is considered active
    Active,
    /// Gene is considered inactive
    Inactive,
}

/// Representation of a Gene Protein Reaction Rule as an AST
pub enum Gpr {
    /// Operation on two genes (see [`GprOperation`])
    Operation(GprOperation),
    /// A terminal gene Node (see [`Gene`])
    Gene(Rc<RefCell<Gene>>),
}

impl Gpr {
    /// Evaluate whether a GPR evaluates to Active or Inactive
    pub fn eval(&self)->GeneActivity{
        match self {
            Gpr::Operation(op) => {
                match op {
                    GprOperation::Or { left,right } => {
                        let l = left.eval();
                        let r = right.eval();
                        if l==GeneActivity::Active || r==GeneActivity::Active {
                            GeneActivity::Active
                        } else {
                            GeneActivity::Inactive
                        }
                    }
                    GprOperation::And { left, right } => {
                        let l = left.eval();
                        let r = right.eval();
                        if l==GeneActivity::Active && r==GeneActivity::Active {
                            GeneActivity::Active
                        } else {
                            GeneActivity::Inactive
                        }
                    }
                    GprOperation::Not { val } => {
                        if val.eval() == GeneActivity::Active {
                            GeneActivity::Inactive
                        } else {
                            GeneActivity::Active
                        }
                    }
                }
            }
            Gpr::Gene(g) => {(**g).borrow().activity.clone()}
        }
    }
}

/// Possible operations on genes
pub enum GprOperation {
    Or{left: Box<Gpr>, right: Box<Gpr>},
    And{left: Box<Gpr>, right: Box<Gpr>},
    Not{val: Box<Gpr>},
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use crate::core::gene::{Gene, GeneActivity, Gpr, GprOperation};

    #[test]
    fn test_gene_node(){
        let active_gene = Gene {
            id: "Active".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene_node = Gpr::Gene(Rc::new(RefCell::new(active_gene)));
        let inactive_gene = Gene {
            id: "Inactive".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene_node = Gpr::Gene(Rc::new(RefCell::new(inactive_gene)));
        assert_eq!(active_gene_node.eval(), GeneActivity::Active);
        assert_eq!(inactive_gene_node.eval(), GeneActivity::Inactive);
    }

    #[test]
    fn test_and_node(){
        // Try an inactive and an active
        let active_gene1 = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene1_ref = Rc::new(RefCell::new(active_gene1));
        let active_gene1_node = Gpr::Gene(active_gene1_ref.clone());
        let inactive_gene2 = Gene {
            id: "Inactive2".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene2_ref = Rc::new(RefCell::new(inactive_gene2));
        let inactive_gene2_node = Gpr::Gene(inactive_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::And {
            left: Box::new(active_gene1_node),
            right: Box::new(inactive_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Inactive);

        // Try Two active
        let active_gene1 = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene1_ref = Rc::new(RefCell::new(active_gene1));
        let active_gene1_node = Gpr::Gene(active_gene1_ref.clone());
        let active_gene2 = Gene {
            id: "Active2".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene2_ref = Rc::new(RefCell::new(active_gene2));
        let active_gene2_node = Gpr::Gene(active_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::And {
            left: Box::new(active_gene1_node),
            right: Box::new(active_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Active);

        // Test two Inactive genes
        let inactive_gene1 = Gene {
            id: "Inactive1".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene1_ref = Rc::new(RefCell::new(inactive_gene1));
        let inactive_gene1_node = Gpr::Gene(inactive_gene1_ref.clone());
        let inactive_gene2 = Gene {
            id: "Inactive2".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene2_ref = Rc::new(RefCell::new(inactive_gene2));
        let inactive_gene2_node = Gpr::Gene(inactive_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::And {
            left: Box::new(inactive_gene1_node),
            right: Box::new(inactive_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Inactive);
    }

    #[test]
    fn test_or_node(){
        // Try an inactive and an active
        let active_gene1 = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene1_ref = Rc::new(RefCell::new(active_gene1));
        let active_gene1_node = Gpr::Gene(active_gene1_ref.clone());
        let inactive_gene2 = Gene {
            id: "Inactive2".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene2_ref = Rc::new(RefCell::new(inactive_gene2));
        let inactive_gene2_node = Gpr::Gene(inactive_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene1_node),
            right: Box::new(inactive_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Active);

        // Try Two active
        let active_gene1 = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene1_ref = Rc::new(RefCell::new(active_gene1));
        let active_gene1_node = Gpr::Gene(active_gene1_ref.clone());
        let active_gene2 = Gene {
            id: "Active2".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene2_ref = Rc::new(RefCell::new(active_gene2));
        let active_gene2_node = Gpr::Gene(active_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(active_gene1_node),
            right: Box::new(active_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Active);

        // Test two Inactive genes
        let inactive_gene1 = Gene {
            id: "Inactive1".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene1_ref = Rc::new(RefCell::new(inactive_gene1));
        let inactive_gene1_node = Gpr::Gene(inactive_gene1_ref.clone());
        let inactive_gene2 = Gene {
            id: "Inactive2".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene2_ref = Rc::new(RefCell::new(inactive_gene2));
        let inactive_gene2_node = Gpr::Gene(inactive_gene2_ref.clone());
        let gpr_and_inactive = Gpr::Operation(GprOperation::Or {
            left: Box::new(inactive_gene1_node),
            right: Box::new(inactive_gene2_node),
        });
        assert_eq!(gpr_and_inactive.eval(), GeneActivity::Inactive);
    }

    #[test]
    fn test_not_node(){
        // Test Active Gene
        let active_gene = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene_ref = Rc::new(RefCell::new(active_gene));
        let active_gene_node = Gpr::Gene(active_gene_ref.clone());
        let active_not = Gpr::Operation(GprOperation::Not{
            val: Box::new(active_gene_node),
        });
        assert_eq!(active_not.eval(), GeneActivity::Inactive);

        // Test Inactive Gene
        let inactive_gene = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Inactive,
        };
        let inactive_gene_ref = Rc::new(RefCell::new(inactive_gene));
        let inactive_gene_node = Gpr::Gene(inactive_gene_ref.clone());
        let inactive_not = Gpr::Operation(GprOperation::Not{
            val: Box::new(inactive_gene_node),
        });
        assert_eq!(inactive_not.eval(), GeneActivity::Active);


    }
}