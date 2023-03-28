use yeter::Database;
use rustre_parser::ast::{AstNode, AstToken, CallByPosExpressionNode, ExpressionNode, NodeNode, TypeNode};
use crate::diagnostics::{Diagnostic, Level, Span};
use crate::TypedSignature;

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub enum Type {
    /// Returned when an identifier cannot be resolved, or from an operator when it cannot resolve
    /// its type due to an operand being unknown to.
    #[default]
    Unknown,

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
    },

    /// Tuple value or return type of a function with multiple (or 0) values
    ///
    /// A tuple **cannot** contain only one element, but **may** be empty. More specifically, if a
    /// function returns exactly one value, it **mustn't** be typed as a `ReturnTuple` as this would
    /// prevent it from being used as an operand to pretty much all operators.
    Tuple(Vec<Type>),
}

impl Type {
    pub fn is_function(&self) -> bool {
        match self {
            Type::Function { .. } => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Type::Array { .. } => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

// This is not great for complex types. For instance, type aliases should remain as-is instead of
// being displayed as their resolved version. We should change that later.
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "{{unknown}}"),
            Type::Boolean => write!(f, "bool"),
            Type::Integer => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function { .. } => write!(f, "{{function}}"),
            Type::Array { elem, size } => write!(f, "{elem}^{size}"),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (idx, ty) in types.iter().enumerate() {
                    if idx == 0 {
                        write!(f, "{ty}")
                    } else {
                        write!(f, ", {ty}")
                    }?;
                }
                write!(f, ")")
            },
        }
    }
}

/// **Query**: Type-checks a given node
#[yeter::query]
pub fn type_check_query(db: &yeter::Database, node_name: String) -> Result<Type, ()> {
    let _node = crate::name_resolution::find_node(db, node_name);
    //for equals_equation in node.unwrap().body_node().unwrap().all_equals_equation_node() {
        //TODO Left node

    //}
    

    todo!()
}

#[yeter::query]
pub fn type_of_ast_type(db: &Database, node: Option<NodeNode>, type_node: TypeNode) -> Type {
    let scalar = if type_node.bool().is_some() {
        Type::Boolean
    } else if type_node.int().is_some() {
        Type::Integer
    } else if type_node.real().is_some() {
        Type::Real
    } else if let Some(id) = type_node.id_node() {
        let decl = crate::name_resolution::resolve_type_decl(db, id.clone());

        match decl.as_ref() {
            Some(decl) => Type::clone(&decl.type_node().map(|t| type_of_ast_type(db, node, t)).unwrap_or_default()),
            None => {
                let span = Span::of_node(db, id.syntax());
                let ident = id.ident().unwrap();
                let name = ident.text();

                Diagnostic::new(Level::Error, format!("cannot resolve type {name:?}"))
                    .with_attachment(span, "not found in this scope")
                    .emit(db);

                Type::Unknown
            },
        }
    } else {
        Type::Unknown
    };

    if let Some(power) = type_node.power() {
        todo!("const-eval array length ({power:?}) using {type_node:?}");
    } else {
        scalar
    }
}

pub fn type_check_expression(db: &yeter::Database, expr: &ExpressionNode) -> Result<Type, ()> {
    match expr {
        ExpressionNode::ConstantNode(constant) => {
            return if constant.is_true() || constant.is_false() {
                Ok(Type::Boolean)
            } else if constant.i_const().is_some() {
                Ok(Type::Integer)
            } else if constant.r_const().is_some() {
                Ok(Type::Real)
            } else {
                Ok(Type::Unknown)
            }
        },
        ExpressionNode::NotExpressionNode(node) => {
            let _exp = type_check_expression(db, &node.operand().unwrap());
        },
        ExpressionNode::NegExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.operand().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::PreExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.operand().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::CurrentExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.operand().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                Ok(Type::Real) => Ok(Type::Real),
                _ => Err(()),
            };
        },
        ExpressionNode::IntExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.operand().unwrap());
            return match type_exp {
                Ok(Type::Integer) => Ok(Type::Integer),
                _ => Err(()),
            };
        },
        ExpressionNode::RealExpressionNode(node) => {
            let type_exp = type_check_expression(db, &node.operand().unwrap());
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
        ExpressionNode::ArrowExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == right_node_type {
                return left_node_type;
            } else {
                return Err(());
            }
        },
        ExpressionNode::AndExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type != Ok(Type::Boolean) {
                return Err(());
            } else if right_node_type != Ok(Type::Boolean) {
                return Err(());
            } else {
                return Ok(Type::Boolean);
            }
        },
        ExpressionNode::OrExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type != Ok(Type::Boolean) {
                return Err(());
            } else if right_node_type != Ok(Type::Boolean) {
                return Err(());
            } else {
                return Ok(Type::Boolean);
            }
        },
        ExpressionNode::XorExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type != Ok(Type::Boolean) {
                return Err(());
            } else if right_node_type != Ok(Type::Boolean) {
                return Err(());
            } else {
                return Ok(Type::Boolean);
            }
        },
        ExpressionNode::ImplExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type != Ok(Type::Boolean) {
                return Err(());
            } else if right_node_type != Ok(Type::Boolean) {
                return Err(());
            } else {
                return Ok(Type::Boolean);
            }
        },
        ExpressionNode::EqExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == right_node_type {
                return left_node_type;
            } else {
                return Err(());
            }
        },
        ExpressionNode::NeqExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == right_node_type {
                return left_node_type;
            } else {
                return Err(());
            }
        },
        ExpressionNode::LtExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::LteExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::GtExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::GteExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::DivExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::ModExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::SubExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::AddExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::MulExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap());
            let right_node_type = type_check_expression(db, &node.right().unwrap());
            if left_node_type == Ok(Type::Integer) && right_node_type == Ok(Type::Integer) {
                return Ok(Type::Integer);
            } else if left_node_type == Ok(Type::Real) && right_node_type == Ok(Type::Real) {
                return Ok(Type::Real);
            } else {
                return Err(());
            }
        },
        ExpressionNode::PowerExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &node.left().unwrap())?;
            let right_node_type = type_check_expression(db, &node.right().unwrap())?;
            if left_node_type.is_array() || right_node_type.is_array() ||
                left_node_type.is_function() || right_node_type.is_function() ||
                left_node_type == Type::Boolean || right_node_type == Type::Boolean {
                return Err(());
            } else if left_node_type == Type::Integer && right_node_type == Type::Integer {
                return Ok(Type::Integer);
            } else {
                return Ok(Type::Real);
            }
        },
        ExpressionNode::IfExpressionNode(node) => {
            let if_body_type = type_check_expression(db, &node.if_body().unwrap())?;
            let else_body_type = type_check_expression(db, &node.else_body().unwrap())?;
            if if_body_type == else_body_type {
                return Ok(if_body_type);
            } else {
                return Err(());
            }
        },
        ExpressionNode::WithExpressionNode(node) => {
            let with_body_type = type_check_expression(db, &node.with_body().unwrap())?;
            let else_body_type = type_check_expression(db, &node.else_body().unwrap())?;
            if with_body_type == else_body_type {
                return Ok(with_body_type);
            } else {
                return Err(());
            }
        },
        ExpressionNode::DieseExpressionNode(node) => {
            let node_list = node.list().unwrap().all_expression_node();
            for element in node_list {
                if type_check_expression(db, &element) != Ok(Type::Boolean) {
                    return Err(());
                }
            }
            return Ok(Type::Boolean);
        },
        ExpressionNode::NorExpressionNode(node) => {
            let node_list = node.list().unwrap().all_expression_node();
            for element in node_list {
                if type_check_expression(db, &element) != Ok(Type::Boolean) {
                    return Err(());
                }
            }
            return Ok(Type::Boolean);
        },
        ExpressionNode::IdentExpressionNode(_node) => {
            todo!("name resolution is required");
        },
        ExpressionNode::ParExpressionNode(node) => {
            return type_check_expression(db, &node.expression_node().unwrap())
        }
        ExpressionNode::CallByPosExpressionNode(expr) => {
            let name = expr.node_ref()
                .and_then(|r| r.id_node())
                .and_then(|i| i.ident());

            if let Some(name) = name {
                let node_node = crate::name_resolution::find_node(db, name.text().into());

                if let Some(node_node) = Option::clone(&node_node) {
                    let sig = crate::get_typed_signature(db, node_node);
                    return Ok(check_call_expression(db, expr, &sig));
                } else {
                    let span = Span::of_token(db, name.syntax());

                    Diagnostic::new(Level::Error, format!("unknown node {:?}", name.text()))
                        .with_attachment(span, "not found in this scope")
                        .emit(db);
                }
            }

            return Ok(Type::Unknown);
        }
    }
    todo!()
}

fn check_call_expression(
    db: &Database,
    expr: &CallByPosExpressionNode,
    sig: &TypedSignature,
) -> Type {
    // Check input parameters
    let expected = sig.params.iter().map(Some).chain(std::iter::repeat(None));
    let found = expr.args().skip(1).map(Some).chain(std::iter::repeat(None));
    for (expected, found) in expected.zip(found) {
        match (expected, found) {
            (None, None) => break,
            (Some((_, expected_ty)), Some(found)) => {
                if !expected_ty.is_unknown() {
                    let found_ty = type_check_expression(db, &found).unwrap_or_default();
                    if !found_ty.is_unknown() && expected_ty != &found_ty {
                        let span = Span::of_node(db, found.syntax());
                        Diagnostic::new(Level::Error, "invalid type for argument")
                            .with_attachment(span, format!("expected {expected_ty}, found {found_ty}"))
                            .emit(db);
                    }
                }
            }
            (Some((expected_ident, _expected_type)), None) => {
                let error_span = expr.args()
                    .last()
                    .map(|s| Span::of_node(db, s.syntax()))
                    .or_else(|| expr.open_par().map(|p| Span::of_token(db, p.syntax())))
                    .or_else(|| expr.node_ref().map(|i| Span::of_node(db, i.syntax())))
                    .unwrap_or_else(|| Span::of_node(db, expr.syntax()))
                    .after();

                let name_span = Span::of_node(db,  expr.node_ref().unwrap().syntax());
                let found_count = expr.args().skip(1).count();
                let expected_count = sig.params.len();
                let expected_ident = expected_ident.text();

                Diagnostic::new(Level::Error, format!("missing argument {expected_ident:?}"))
                    .with_attachment(name_span, format!("this function expects {expected_count} arguments but {found_count} were supplied"))
                    .with_attachment(error_span, "hint: add the missing arguments(s)")
                    .emit(db);
            }
            (None, Some(found)) => {
                let arg_span = Span::of_node(db, found.syntax());
                let name_span = Span::of_node(db,  expr.node_ref().unwrap().syntax());
                let found_count = expr.args().skip(1).count();
                let expected_count = sig.params.len();

                Diagnostic::new(Level::Error, "unexpected argument")
                    .with_attachment(arg_span, "hint: remove this argument")
                    .with_attachment(name_span, format!("this function expects {expected_count} arguments but {found_count} were supplied"))
                    .emit(db);
            }
        }
    }

    // Find out returned value(s)
    if sig.return_params.len() == 1 {
        sig.return_params[0].1.clone()
    } else {
        let cloned = sig.return_params.iter().map(|(_, t)| t).cloned().collect();
        Type::Tuple(cloned)
    }
}
