use rustre_parser::ast::*;
use yeter::Database;

use crate::{types::ConstValue, name_resolution::NameResolveQuery};

#[yeter::query]
pub fn eval_const_node(db: &Database, node: ExpressionNode) -> Option<ConstValue> {
    // TODO : Parse constant nodes values from string better
    match node {
        ExpressionNode::ConstantNode(node) => {
            if(node.r#true().is_some()) {
                return Some(ConstValue::Boolean(true));
            }
            if(node.r#false().is_some()) {
                return Some(ConstValue::Boolean(false));
            }
            if(node.i_const().is_some()) {
                return Some(ConstValue::Integer(node.i_const().unwrap().text().parse::<i32>().unwrap()));
            }
            if(node.r_const().is_some()) {
                return Some(ConstValue::Real(node.r_const().unwrap().text().parse::<f32>().unwrap()));
            }
        },
        ExpressionNode::IdentExpressionNode(_) => todo!(),
        ExpressionNode::NotExpressionNode(_) => todo!(),
        ExpressionNode::NegExpressionNode(_) => todo!(),
        ExpressionNode::PreExpressionNode(_) => todo!(),
        ExpressionNode::CurrentExpressionNode(_) => todo!(),
        ExpressionNode::IntExpressionNode(_) => todo!(),
        ExpressionNode::RealExpressionNode(_) => todo!(),
        ExpressionNode::WhenExpressionNode(_) => todo!(),
        ExpressionNode::FbyExpressionNode(_) => todo!(),
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
        ExpressionNode::AddExpressionNode(node) => {
            let left = eval_const_node(db, node.left()?.clone())?;
            let right = eval_const_node(db, node.right()?.clone())?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left + right));
                },
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left + right));
                },
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 + right));
                },
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left + right as f32));
                },
                _ => todo!(),
            }
        },
        ExpressionNode::MulExpressionNode(_) => todo!(),
        ExpressionNode::PowerExpressionNode(_) => todo!(),
        ExpressionNode::IfExpressionNode(_) => todo!(),
        ExpressionNode::WithExpressionNode(_) => todo!(),
        ExpressionNode::DieseExpressionNode(_) => todo!(),
        ExpressionNode::NorExpressionNode(_) => todo!(),
        ExpressionNode::ParExpressionNode(_) => todo!(),
    }

    Some(ConstValue::Integer(0))
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use super::eval_const_node;

    #[test]
    fn one_plus_one() {
        let db = crate::driver();
        let node = crate::parse_file(&db, crate::SourceFile { path: PathBuf::new(), text: String::from("const x = 1 + 1;") });
        let expr = node.all_constant_decl_node().next().unwrap().all_one_constant_decl_node().next().unwrap().expression_node().unwrap();
        let value = eval_const_node(&db, expr);
        let value = value.as_ref().as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Integer(2));
    }
}