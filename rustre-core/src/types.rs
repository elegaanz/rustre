use rustre_parser::ast::ExpressionNode;

#[derive(PartialEq, Eq)]
pub enum Type {
    Boolean,
    Integer,
    Real,
    Function {
        args: Vec<Type>,
        ret: Vec<Type>,
    },
    Array {
        elem: Box<Type>,
        size: usize,
    }
}

/// **Query**: Type-checks a given node
#[yeter::query]
pub fn type_check_query(db: &yeter::Database, node_name: String) -> Result<Type, ()> {
    let _node = crate::find_node(db, node_name);
    //for equals_equation in node.unwrap().body_node().unwrap().all_equals_equation_node() {
        //TODO Left node

    //}
    

    todo!()
}

pub fn type_check_expression(db: &yeter::Database, expr: &ExpressionNode) -> Result<Type, ()> {
    match expr {
        ExpressionNode::ConstantNode(node) => {
            todo!()
        },
        ExpressionNode::NotExpressionNode(node) => {
            let exp = type_check_expression(db, &node.expression_node().unwrap());
        },
        ExpressionNode::NegExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.expression_node().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::PreExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.expression_node().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::CurrentExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.expression_node().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::IntExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.expression_node().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                _ => Err(()),
            };
        },
        ExpressionNode::RealExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.expression_node().unwrap());
            return match type_exp {
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::WhenExpressionNode(_) => todo!(),
        ExpressionNode::FbyExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == right_node_type {
                return left_node_type;
            } else {
                return Err(());
            }
        },
        ExpressionNode::ArrowExpressionNode(_) => todo!(),
        ExpressionNode::AndExpressionNode(_) => todo!(),
        ExpressionNode::OrExpressionNode(_) => todo!(),
        ExpressionNode::XorExpressionNode(_) => todo!(),
        ExpressionNode::ImplExpressionNode(_) => todo!(),
        ExpressionNode::EqExpressionNode(_) => todo!(),
        ExpressionNode::NeqExpressionNode(_) => todo!(),
        ExpressionNode::LtExpressionNode(_) => todo!(),
        ExpressionNode::LteExpressionNode(_) => todo!(),
        ExpressionNode::GtExpressionNode(_) => todo!(),
        ExpressionNode::GteExpressionNode(_) => todo!(),
        ExpressionNode::DivExpressionNode(_) => todo!(),
        ExpressionNode::ModExpressionNode(_) => todo!(),
        ExpressionNode::SubExpressionNode(_) => todo!(),
        ExpressionNode::AddExpressionNode(_) => todo!(),
        ExpressionNode::MulExpressionNode(_) => todo!(),
        ExpressionNode::PowerExpressionNode(_) => todo!(),
        ExpressionNode::IfExpressionNode(_) => todo!(),
        ExpressionNode::WithExpressionNode(_) => todo!(),
        ExpressionNode::DieseExpressionNode(_) => todo!(),
        ExpressionNode::NorExpressionNode(_) => todo!(),
        ExpressionNode::ParExpressionNode(_) => todo!(),
        ExpressionNode::IdentExpressionNode(_) => todo!(),
    }
    todo!()
}
