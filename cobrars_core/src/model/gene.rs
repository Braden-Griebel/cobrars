//! This module provides the Gene struct, representing a gene, and the GPR struct, representing a
//! gene protein reaction rule
use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

/// Structure Representing a Gene
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Gene {
    /// Used to identify the gene
    pub id: String,
    /// Human Readable Gene Name
    name: Option<String>,
    /// Whether this gene is currently active (see [`GeneActivity`])
    activity: GeneActivity,
}

impl Gene {
    pub fn new(id: String, name: Option<String>, activity: GeneActivity) -> Gene {
        Gene { id, name, activity }
    }
}

impl Display for Gene {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(s) => write!(f, "{}", s),
            None => write!(f, "{}", self.id),
        }
    }
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
    /// Create a new binary operation node
    pub fn new_binary_operation(
        left: Gpr,
        operator: GprOperatorType,
        right: Gpr,
    ) -> Result<Gpr, GprError> {
        let op = match operator {
            GprOperatorType::Or => GprOperation::Or {
                left: Box::new(left),
                right: Box::new(right),
            },
            GprOperatorType::And => GprOperation::And {
                left: Box::new(left),
                right: Box::new(right),
            },
            GprOperatorType::Not => return Err(GprError::InvalidBinaryOp),
        };
        Ok(Gpr::Operation(op))
    }

    /// Create a new unary operation node
    pub fn new_unary_operation(operator: GprOperatorType, operand: Gpr) -> Result<Gpr, GprError> {
        let op = match operator {
            GprOperatorType::Not => GprOperation::Not {
                val: Box::new(operand),
            },
            _ => return Err(GprError::InvalidUnaryOp),
        };
        Ok(Gpr::Operation(op))
    }

    /// Create a new gene node
    pub fn new_gene_node(gene: Rc<RefCell<Gene>>) -> Gpr {
        Gpr::Gene(gene)
    }

    /// Evaluate whether a GPR evaluates to Active or Inactive
    pub fn eval(&self) -> GeneActivity {
        match self {
            Gpr::Operation(op) => match op {
                GprOperation::Or { left, right } => {
                    let l = left.eval();
                    let r = right.eval();
                    if l == GeneActivity::Active || r == GeneActivity::Active {
                        GeneActivity::Active
                    } else {
                        GeneActivity::Inactive
                    }
                }
                GprOperation::And { left, right } => {
                    let l = left.eval();
                    let r = right.eval();
                    if l == GeneActivity::Active && r == GeneActivity::Active {
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
            },
            Gpr::Gene(g) => (**g).borrow().activity.clone(),
        }
    }

    /// Generate a GPR string with gene ids from the GPR AST
    pub fn to_string_id(&self) -> String {
        match self {
            Gpr::Operation(op) => match op {
                GprOperation::Or { left, right } => {
                    format!("({} or {})", left.to_string_id(), right.to_string_id())
                }
                GprOperation::And { left, right } => {
                    format!("({} and {})", left.to_string_id(), right.to_string_id())
                }
                GprOperation::Not { val } => {
                    format!("(not {})", val)
                }
            },
            Gpr::Gene(gene_ref) => (**gene_ref).borrow().id.clone(),
        }
    }
}

impl Display for Gpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_id())
    }
}

/// Possible operations on genes
pub enum GprOperation {
    Or { left: Box<Gpr>, right: Box<Gpr> },
    And { left: Box<Gpr>, right: Box<Gpr> },
    Not { val: Box<Gpr> },
}

pub enum GprOperatorType {
    Or,
    And,
    Not,
}

pub enum GprError {
    InvalidBinaryOp,
    InvalidUnaryOp,
}

#[cfg(test)]
mod tests {
    use crate::model::gene::{Gene, GeneActivity, Gpr, GprOperation};
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_gene_node() {
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
    fn test_and_node() {
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
    fn test_or_node() {
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
    fn test_not_node() {
        // Test Active Gene
        let active_gene = Gene {
            id: "Active1".to_string(),
            name: None,
            activity: GeneActivity::Active,
        };
        let active_gene_ref = Rc::new(RefCell::new(active_gene));
        let active_gene_node = Gpr::Gene(active_gene_ref.clone());
        let active_not = Gpr::Operation(GprOperation::Not {
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
        let inactive_not = Gpr::Operation(GprOperation::Not {
            val: Box::new(inactive_gene_node),
        });
        assert_eq!(inactive_not.eval(), GeneActivity::Active);
    }
}
