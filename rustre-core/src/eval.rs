use crate::{
    name_resolution::{self, NameResolveQuery},
    types::ConstValue,
};
use rustre_parser::ast::*;
use yeter::Database;

#[yeter::query]
pub fn eval_const_node(
    db: &Database,
    node: ExpressionNode,
    in_node: Option<NodeNode>,
) -> Option<ConstValue> {
    // TODO : Parse constant nodes values from string better
    match node {
        ExpressionNode::ConstantNode(node) => {
            if node.r#true().is_some() {
                return Some(ConstValue::Boolean(true));
            }
            if node.r#false().is_some() {
                return Some(ConstValue::Boolean(false));
            }
            if node.i_const().is_some() {
                return Some(ConstValue::Integer(
                    node.i_const().unwrap().text().parse::<i32>().unwrap(),
                ));
            }
            if node.r_const().is_some() {
                return Some(ConstValue::Real(
                    node.r_const().unwrap().text().parse::<f32>().unwrap(),
                ));
            }
        }
        ExpressionNode::IdentExpressionNode(node) => {
            let ident = node.id_node().unwrap();
            let node = name_resolution::resolve_const_expr_node(
                db,
                NameResolveQuery {
                    ident: ident.ident().unwrap(),
                    in_node: in_node.clone(),
                },
            );
            if let Some(ref node) = node.as_ref() {
                return Option::clone(&eval_const_node(db, node.clone(), in_node.clone()));
            }
            return None;
        }
        ExpressionNode::NotExpressionNode(node) => {
            let value = Option::clone(&eval_const_node(
                db,
                node.operand()?.clone(),
                in_node.clone(),
            ))?;
            match value {
                ConstValue::Boolean(value) => {
                    return Some(ConstValue::Boolean(!value));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::NegExpressionNode(node) => {
            let value = Option::clone(&eval_const_node(
                db,
                node.operand()?.clone(),
                in_node.clone(),
            ))?;
            match value {
                ConstValue::Integer(value) => {
                    return Some(ConstValue::Integer(-value));
                }
                ConstValue::Real(value) => {
                    return Some(ConstValue::Real(-value));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::PreExpressionNode(_) => todo!(),
        ExpressionNode::CurrentExpressionNode(_) => todo!(),
        ExpressionNode::IntExpressionNode(node) => {
            let value = Option::clone(&eval_const_node(
                db,
                node.operand()?.clone(),
                in_node.clone(),
            ))?;
            match value {
                ConstValue::Integer(value) => {
                    return Some(ConstValue::Integer(value));
                }
                ConstValue::Real(value) => {
                    return Some(ConstValue::Integer(value as i32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::RealExpressionNode(node) => {
            let value = Option::clone(&eval_const_node(
                db,
                node.operand()?.clone(),
                in_node.clone(),
            ))?;
            match value {
                ConstValue::Integer(value) => {
                    return Some(ConstValue::Real(value as f32));
                }
                ConstValue::Real(value) => {
                    return Some(ConstValue::Real(value));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::WhenExpressionNode(_) => todo!(),
        ExpressionNode::FbyExpressionNode(_) => todo!(),
        ExpressionNode::ArrowExpressionNode(_) => todo!(),
        ExpressionNode::AndExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(
                db,
                node.left()?.clone(),
                in_node.clone().clone(),
            ))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(left && right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::OrExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(left || right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::XorExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(left ^ right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::ImplExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(!left || right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::EqExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(left == right));
                }
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left == right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left == right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::NeqExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Boolean(left), ConstValue::Boolean(right)) => {
                    return Some(ConstValue::Boolean(left != right));
                }
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left != right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left != right));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::LtExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left < right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left < right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean((left as f32) < right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left < (right as f32)));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::LteExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left <= right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left <= right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean((left as f32) <= right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left <= (right as f32)));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::GtExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left > right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left > right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean((left as f32) > right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left > (right as f32)));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::GteExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left >= right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean(left >= right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Boolean((left as f32) >= right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Boolean(left >= (right as f32)));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::DivExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left / right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left / right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 / right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left / right as f32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::ModExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left % right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left % right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 % right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left % right as f32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::SubExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left - right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left - right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 - right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left - right as f32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::AddExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left + right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left + right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 + right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left + right as f32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::MulExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left * right));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left * right));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left as f32 * right));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left * right as f32));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::PowerExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match (left, right) {
                (ConstValue::Integer(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Integer(left.pow(right as u32)));
                }
                (ConstValue::Real(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real(left.powf(right)));
                }
                (ConstValue::Integer(left), ConstValue::Real(right)) => {
                    return Some(ConstValue::Real((left as f32).powf(right)));
                }
                (ConstValue::Real(left), ConstValue::Integer(right)) => {
                    return Some(ConstValue::Real(left.powf(right as f32)));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::IfExpressionNode(node) => {
            let cond = Option::clone(&eval_const_node(db, node.cond()?.clone(), in_node.clone()))?;
            match cond {
                ConstValue::Boolean(true) => {
                    return Option::clone(&eval_const_node(
                        db,
                        node.if_body()?.clone(),
                        in_node.clone(),
                    ));
                }
                ConstValue::Boolean(false) => {
                    return Option::clone(&eval_const_node(
                        db,
                        node.else_body()?.clone(),
                        in_node.clone(),
                    ));
                }
                _ => todo!(),
            }
        }
        ExpressionNode::HatExpressionNode(node) => {
            let left = Option::clone(&eval_const_node(db, node.left()?.clone(), in_node.clone()))?;
            let right =
                Option::clone(&eval_const_node(db, node.right()?.clone(), in_node.clone()))?;
            match right {
                ConstValue::Integer(i) => {
                    return Some(ConstValue::Array(
                        std::iter::repeat(left).take(i as usize).collect(),
                    ));
                }
                _ => {
                    return None;
                }
            }
        }
        ExpressionNode::WithExpressionNode(_) => todo!(),
        ExpressionNode::DieseExpressionNode(_) => todo!(),
        ExpressionNode::NorExpressionNode(_) => todo!(),
        ExpressionNode::ParExpressionNode(_) => todo!(),
        ExpressionNode::CallByPosExpressionNode(_) => todo!(),
    }

    Some(ConstValue::Integer(0))
}

#[cfg(test)]
mod tests {
    use crate::files;
    use std::path::PathBuf;

    use super::eval_const_node;

    #[test]
    fn one_plus_one() {
        let mut db = crate::driver();
        crate::add_source_contents(&mut db, String::from("const x = 1 + 1;"));
        let node = crate::parse_file(
            &db,
            files(&db)
                .as_ref()
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap()
                .clone(),
        );
        let expr = node
            .all_constant_decl_node()
            .next()
            .unwrap()
            .all_one_constant_decl_node()
            .next()
            .unwrap()
            .expression_node()
            .unwrap();
        let value = Option::clone(&eval_const_node(&db, expr, None));
        let value = value.as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Integer(2));
    }

    #[test]
    fn not_true() {
        let db = crate::driver();
        let node = crate::parse_file(
            &db,
            crate::SourceFile {
                path: PathBuf::new(),
                text: String::from("const x = not true;"),
            },
        );
        let expr = node
            .all_constant_decl_node()
            .next()
            .unwrap()
            .all_one_constant_decl_node()
            .next()
            .unwrap()
            .expression_node()
            .unwrap();
        let value = Option::clone(&eval_const_node(&db, expr, None));
        let value = value.as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Boolean(false));
    }

    #[test]
    fn one_plus_x() {
        let mut db = crate::driver();
        //let node = crate::parse_file(&db, crate::SourceFile { path: PathBuf::new(), text: String::from("const x = 1; const y = 2 + x;") });
        crate::add_source_contents(&mut db, String::from("const x = 1;\nconst y = 2 + x;\n"));
        let node = crate::parse_file(
            &db,
            files(&db)
                .as_ref()
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap()
                .clone(),
        );
        let expr = node
            .all_constant_decl_node()
            .last()
            .unwrap()
            .all_one_constant_decl_node()
            .next()
            .unwrap()
            .expression_node()
            .unwrap();
        let value = Option::clone(&eval_const_node(&db, expr, None));
        let value = value.as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Integer(3));
    }

    #[test]
    fn if_true() {
        let mut db = crate::driver();
        crate::add_source_contents(
            &mut db,
            String::from("const x = true; const y = if x then 1 else 2;"),
        );
        let node = crate::parse_file(
            &db,
            files(&db)
                .as_ref()
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap()
                .clone(),
        );
        let expr = node
            .all_constant_decl_node()
            .last()
            .unwrap()
            .all_one_constant_decl_node()
            .next()
            .unwrap()
            .expression_node()
            .unwrap();
        let value = Option::clone(&eval_const_node(&db, expr, None));
        let value = value.as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Integer(1));
    }

    #[test]
    fn if_false() {
        let mut db = crate::driver();
        crate::add_source_contents(
            &mut db,
            String::from("const x = false; const y = if x then 1 else 2;"),
        );
        let node = crate::parse_file(
            &db,
            files(&db)
                .as_ref()
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap()
                .clone(),
        );
        let expr = node
            .all_constant_decl_node()
            .last()
            .unwrap()
            .all_one_constant_decl_node()
            .next()
            .unwrap()
            .expression_node()
            .unwrap();
        let value = Option::clone(&eval_const_node(&db, expr, None));
        let value = value.as_ref().unwrap();
        assert_eq!(*value, crate::types::ConstValue::Integer(2));
    }
}
