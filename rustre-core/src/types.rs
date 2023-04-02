use crate::diagnostics::{Diagnostic, Level, Span};
use crate::eval::eval_const_node;
use crate::name_resolution::{resolve_runtime_node, NameResolveQuery, ResolvedRuntimeNode};
use crate::TypedSignature;
use rustre_parser::ast::{
    AstNode, AstToken, CallByPosExpressionNode, ExpressionNode, LeftItemNode, NodeNode, TypeNode,
};
use yeter::Database;

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

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    Boolean(bool),
    Integer(i32),
    Real(f32),
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
            }
        }
    }
}

/// **Query**: Type-checks a given node
#[yeter::query]
pub fn type_check_query(db: &yeter::Database, node_node: NodeNode) -> Type {
    let body_node = node_node.body_node();
    let in_node = Some(node_node.clone());

    for node in body_node.as_ref().unwrap().all_equals_equation_node() {
        if let (Some(left_node), Some(expr_node)) = (node.left_node(), node.expression_node()) {
            let lefts = left_node.all_left_item_node();
            let mut left_types = Vec::new();
            for left in lefts {
                let left_type = type_check_left(db, &left, &in_node);
                left_types.push(left_type);
            }
            let left_types = if left_types.len() == 1 {
                left_types.pop().unwrap()
            } else {
                Type::Tuple(left_types)
            };

            let right_types = type_check_expression(db, &expr_node, &in_node);

            if left_types != right_types {
                Diagnostic::new(Level::Error, "incompatible types")
                    .with_attachment(
                        Span::of_node(db, node.left_node().unwrap().syntax()),
                        format!("the left term is of type {}", left_types),
                    )
                    .with_attachment(
                        Span::of_node(db, node.expression_node().unwrap().syntax()),
                        format!("while the right term is of type {}", right_types),
                    )
                    .emit(db);
            }
        }
    }

    for node in body_node.as_ref().unwrap().all_assert_equation_node() {
        let right_types = type_check_expression(db, &node.expression_node().unwrap(), &in_node);

        if right_types != Type::Boolean {
            Diagnostic::new(Level::Error, "assertions should be boolean expressions")
                .with_attachment(
                    Span::of_node(db, node.expression_node().unwrap().syntax()),
                    format!("this expression has type {}", right_types),
                )
                .emit(db);
        }
    }

    let node_profile_node = node_node.node_profile_node();
    let mut args = Vec::new();
    let mut ret = Vec::new();

    let params = node_profile_node
        .as_ref()
        .as_ref()
        .unwrap()
        .params()
        .unwrap()
        .all_var_decl_node();
    for param in params {
        let typed_id_nodes = param.all_typed_ids_node();
        for type_nodes in typed_id_nodes {
            args.push(type_of_ast_type(db, in_node.clone(), type_nodes.type_node().unwrap()).as_ref().clone());
        }
    }

    let return_params = node_profile_node
        .as_ref()
        .as_ref()
        .unwrap()
        .return_params()
        .unwrap()
        .all_var_decl_node();
    for return_param in return_params {
        let typed_id_nodes = return_param.all_typed_ids_node();
        for type_nodes in typed_id_nodes {
            ret.push(type_of_ast_type(db, in_node.clone(), type_nodes.type_node().unwrap()).as_ref().clone());
        }
    }
    Type::Function { args, ret }
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
            Some(decl) => Type::clone(
                &decl
                    .type_node()
                    .map(|t| type_of_ast_type(db, node.clone(), t))
                    .unwrap_or_default(),
            ),
            None => {
                let span = Span::of_node(db, id.syntax());
                let ident = id.ident().unwrap();
                let name = ident.text();

                Diagnostic::new(Level::Error, format!("cannot resolve type {name:?}"))
                    .with_attachment(span, "not found in this scope")
                    .emit(db);

                Type::Unknown
            }
        }
    } else {
        Type::Unknown
    };

    if let Some(power) = type_node.power() {
        let size = match *eval_const_node(db, power, node) {
            Some(ConstValue::Integer(i)) => i as usize,
            _ => return Type::Unknown,
        };
        Type::Array { elem: Box::new(scalar), size }
    } else {
        scalar
    }
}

macro_rules! some_or_unknown {
    ($option:expr) => {
        match $option {
            Some(x) => x,
            _ => return Type::Unknown,
        }
    };
}

/// Resolves the declared type of an ident using a [NameResolveQuery] from [crate::name_resolution]
#[yeter::query]
pub fn declared_type_of_ident(db: &Database, query: NameResolveQuery) -> Option<Type> {
    let in_node = query.in_node.clone();
    let resolved_node = crate::name_resolution::resolve_runtime_node(db, query);
    let resolved_node = resolved_node.as_ref().as_ref()?;

    Some(match resolved_node {
        ResolvedRuntimeNode::Const(const_decl_node) => {
            type_of_ast_type(db, in_node, const_decl_node.type_node()?).as_ref().clone()
        }
        ResolvedRuntimeNode::Param(var_decl_node)
        | ResolvedRuntimeNode::ReturnParam(var_decl_node)
        | ResolvedRuntimeNode::Var(var_decl_node) => {
            type_of_ast_type(db, in_node, var_decl_node.type_node()?).as_ref().clone()
        }
    })
}

macro_rules! ty_check_expr {
    (unary, $db:expr, $node:expr, $in_node:expr, $message:expr, $accept:pat) => {{
        let operand = some_or_unknown!($node.operand());
        let type_exp = type_check_expression($db, &operand, $in_node);
        match type_exp {
            $accept => type_exp,
            _ => {
                Diagnostic::new(Level::Error, $message)
                    .with_attachment(
                        Span::of_node($db, operand.syntax()),
                        format!("this expression is of type {}", type_exp),
                    )
                    .emit($db);
                Type::Unknown
            }
        }
    }};
    (binary, $db:expr, $node:expr, $in_node:expr) => {{
        let left_node_type = type_check_expression($db, &some_or_unknown!($node.left()), $in_node);
        let right_node_type = type_check_expression($db, &some_or_unknown!($node.right()), $in_node);
        if left_node_type == right_node_type {
            left_node_type
        } else {
            Diagnostic::new(Level::Error, "incompatible types")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.left()).syntax()),
                    format!("this is of type {}", left_node_type),
                )
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.right()).syntax()),
                    format!("while this is of type {}", right_node_type),
                )
                .emit($db);
            Type::Unknown
        }
    }};
    (binary, $db:expr, $node:expr, $in_node:expr, $expect:expr) => {{
        let left_node_type = type_check_expression($db, &some_or_unknown!($node.left()), $in_node);
        let right_node_type = type_check_expression($db, &some_or_unknown!($node.right()), $in_node);

        if left_node_type != $expect {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, $node.left().unwrap().syntax()),
                    format!("expected {}, found {}", $expect, left_node_type),
                )
                .emit($db);
        }

        if right_node_type != $expect {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, $node.right().unwrap().syntax()),
                    format!("expected {}, found {}", $expect, right_node_type),
                )
                .emit($db);
        }

        $expect
    }};
    (comparator, $db:expr, $node:expr, $in_node:expr) => {{
        let left_node_type = type_check_expression($db, &some_or_unknown!($node.left()), $in_node);
        let right_node_type = type_check_expression($db, &some_or_unknown!($node.right()), $in_node);

        if left_node_type != Type::Integer && left_node_type != Type::Real {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.left()).syntax()),
                    format!("expected int or real, found {}", left_node_type),
                )
                .emit($db);
        }

        if right_node_type != Type::Integer && right_node_type != Type::Real {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.right()).syntax()),
                    format!("expected int or real, found {}", right_node_type),
                )
                .emit($db);
        }

        if left_node_type != right_node_type {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.right()).syntax()),
                    format!(
                        "expected {} (because of the left operand), found {} (you can use the `{}` function if you want to do a conversion)",
                        left_node_type, right_node_type, left_node_type,
                    ),
                )
                .emit($db);
        }

        Type::Boolean
    }};
    (binary_number, $db:expr, $node:expr, $in_node:expr) => {{
        let left_node_type = type_check_expression($db, &some_or_unknown!($node.left()), $in_node);
        let right_node_type = type_check_expression($db, &some_or_unknown!($node.right()), $in_node);

        if left_node_type != Type::Integer && left_node_type != Type::Real {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.left()).syntax()),
                    format!("expected int or real, found {}", left_node_type),
                )
                .emit($db);
        }

        if right_node_type != Type::Integer && right_node_type != Type::Real {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.right()).syntax()),
                    format!("expected int or real, found {}", right_node_type),
                )
                .emit($db);
        }

        if left_node_type != right_node_type {
            Diagnostic::new(Level::Error, "incorrect type")
                .with_attachment(
                    Span::of_node($db, some_or_unknown!($node.right()).syntax()),
                    format!(
                        "expected {} (because of the left operand), found {} (you can use the `{}` function if you want to do a conversion)",
                        left_node_type, right_node_type, left_node_type,
                    ),
                )
                .emit($db);
        }

        left_node_type
    }}
}

pub fn type_check_expression(
    db: &yeter::Database,
    expr: &ExpressionNode,
    in_node: &Option<NodeNode>,
) -> Type {
    match expr {
        ExpressionNode::ConstantNode(constant) => {
            return if constant.is_true() || constant.is_false() {
                Type::Boolean
            } else if constant.i_const().is_some() {
                Type::Integer
            } else if constant.r_const().is_some() {
                Type::Real
            } else {
                Type::Unknown
            }
        }
        ExpressionNode::NotExpressionNode(node) => {
            type_check_expression(db, &node.operand().unwrap(), in_node)
        }
        ExpressionNode::NegExpressionNode(node) => ty_check_expr!(
            unary,
            db,
            node,
            in_node,
            "The `-` operator can only be used on int and real",
            Type::Integer | Type::Real
        ),
        ExpressionNode::PreExpressionNode(node) => {
            type_check_expression(db, &node.operand().unwrap(), in_node)
        }
        ExpressionNode::CurrentExpressionNode(node) => {
            type_check_expression(db, &node.operand().unwrap(), in_node)
        }
        ExpressionNode::IntExpressionNode(node) => {
            // TODO: what types can be converted to int?
            let type_exp = type_check_expression(db, &node.operand().unwrap(), in_node);
            match type_exp {
                Type::Real => Type::Integer,
                Type::Integer => {
                    Diagnostic::new(Level::Warning, "useless type conversion")
                        .with_attachment(
                            Span::of_node(db, node.operand().unwrap().syntax()),
                            "this expression is already an int",
                        )
                        .emit(db);
                    Type::Integer
                }
                _ => {
                    Diagnostic::new(Level::Error, "invalid type conversion")
                        .with_attachment(
                            Span::of_node(db, node.operand().unwrap().syntax()),
                            format!(
                                "this expression has type {}, which cannot be converted to int.",
                                type_exp
                            ),
                        )
                        .emit(db);
                    Type::Unknown
                }
            }
        }
        ExpressionNode::RealExpressionNode(node) => {
            // TODO: what types can be converted to real?
            let type_exp = type_check_expression(db, &node.operand().unwrap(), in_node);
            match type_exp {
                Type::Integer => Type::Real,
                Type::Real => {
                    Diagnostic::new(Level::Warning, "useless type conversion")
                        .with_attachment(
                            Span::of_node(db, node.operand().unwrap().syntax()),
                            "this expression is already a real",
                        )
                        .emit(db);
                    Type::Real
                }
                _ => {
                    Diagnostic::new(Level::Error, "invalid type conversion")
                        .with_attachment(
                            Span::of_node(db, node.operand().unwrap().syntax()),
                            format!(
                                "this expression has type {}, which cannot be converted to real.",
                                type_exp
                            ),
                        )
                        .emit(db);
                    Type::Unknown
                }
            }
        }
        ExpressionNode::WhenExpressionNode(_) => todo!(),
        ExpressionNode::FbyExpressionNode(node) => ty_check_expr!(binary, db, node, in_node),
        ExpressionNode::ArrowExpressionNode(node) => ty_check_expr!(binary, db, node, in_node),
        ExpressionNode::AndExpressionNode(node) => {
            ty_check_expr!(binary, db, node, in_node, Type::Boolean)
        }
        ExpressionNode::OrExpressionNode(node) => {
            ty_check_expr!(binary, db, node, in_node, Type::Boolean)
        }
        ExpressionNode::XorExpressionNode(node) => {
            ty_check_expr!(binary, db, node, in_node, Type::Boolean)
        }
        ExpressionNode::ImplExpressionNode(node) => {
            ty_check_expr!(binary, db, node, in_node, Type::Boolean)
        } // TODO, is that true?
        ExpressionNode::EqExpressionNode(node) => ty_check_expr!(binary, db, node, in_node),
        ExpressionNode::NeqExpressionNode(node) => ty_check_expr!(binary, db, node, in_node),
        ExpressionNode::LtExpressionNode(node) => ty_check_expr!(comparator, db, node, in_node),
        ExpressionNode::LteExpressionNode(node) => ty_check_expr!(comparator, db, node, in_node),
        ExpressionNode::GtExpressionNode(node) => ty_check_expr!(comparator, db, node, in_node),
        ExpressionNode::GteExpressionNode(node) => ty_check_expr!(comparator, db, node, in_node),
        ExpressionNode::DivExpressionNode(node) => ty_check_expr!(binary_number, db, node, in_node),
        ExpressionNode::ModExpressionNode(node) => ty_check_expr!(binary_number, db, node, in_node),
        ExpressionNode::SubExpressionNode(node) => ty_check_expr!(binary_number, db, node, in_node),
        ExpressionNode::AddExpressionNode(node) => ty_check_expr!(binary_number, db, node, in_node),
        ExpressionNode::MulExpressionNode(node) => ty_check_expr!(binary_number, db, node, in_node),
        ExpressionNode::PowerExpressionNode(node) => {
            let left_node_type = type_check_expression(db, &some_or_unknown!(node.left()), in_node);
            let right_node_type = type_check_expression(db, &some_or_unknown!(node.right()), in_node);
            let size = match *eval_const_node(db, some_or_unknown!(node.right()), in_node.clone()) {
                Some(ConstValue::Integer(i)) => Some(i as usize),
                _ => None,
            };

            if left_node_type.is_function() {
                Diagnostic::new(Level::Error, "incorrect type")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.left()).syntax()),
                        "cannot build an array of function, do you want to call it first?",
                    )
                    .emit(db);
            }

            if right_node_type != Type::Integer {
                Diagnostic::new(Level::Error, "incorrect type")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.right()).syntax()),
                        format!("expected int, found {}", right_node_type),
                    )
                    .emit(db);
            }

            Type::Array {
                elem: Box::new(left_node_type),
                size: some_or_unknown!(size),
            }
        }
        ExpressionNode::IfExpressionNode(node) => {
            let if_body_type = type_check_expression(db, &some_or_unknown!(node.if_body()), in_node);
            let else_body_type = type_check_expression(db, &some_or_unknown!(node.else_body()), in_node);
            let cond_type = type_check_expression(db, &some_or_unknown!(node.cond()), in_node);

            if if_body_type != else_body_type {
                Diagnostic::new(Level::Error, "incompatible types")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.else_body()).syntax()),
                        format!(
                            "expected {} (because of if body), found {}",
                            if_body_type, else_body_type
                        ),
                    )
                    .emit(db);
            }

            if cond_type != Type::Boolean {
                Diagnostic::new(Level::Error, "Incorrect type")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.cond()).syntax()),
                        format!("expected a boolean condition, found {}", cond_type),
                    )
                    .emit(db);
            }

            if_body_type
        }
        ExpressionNode::WithExpressionNode(node) => {
            let with_body_type = type_check_expression(db, &some_or_unknown!(node.with_body()), in_node);
            let else_body_type = type_check_expression(db, &some_or_unknown!(node.else_body()), in_node);
            let cond_type = type_check_expression(db, &some_or_unknown!(node.cond()), in_node);

            if with_body_type != else_body_type {
                Diagnostic::new(Level::Error, "incompatible types")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.else_body()).syntax()),
                        format!(
                            "expected {} (because of if body), found {}",
                            with_body_type, else_body_type
                        ),
                    )
                    .emit(db);
            }

            if cond_type != Type::Boolean {
                Diagnostic::new(Level::Error, "Incorrect type")
                    .with_attachment(
                        Span::of_node(db, some_or_unknown!(node.cond()).syntax()),
                        format!("expected a boolean condition, found {}", cond_type),
                    )
                    .emit(db);
            }

            with_body_type
        }
        ExpressionNode::DieseExpressionNode(node) => {
            let node_list = node.list().unwrap().all_expression_node();
            for element in node_list {
                let el_type = type_check_expression(db, &element, in_node);
                if el_type != Type::Boolean || el_type != Type::Unknown {
                    Diagnostic::new(Level::Error, "Incorrect type")
                        .with_attachment(
                            Span::of_node(db, element.syntax()),
                            format!("expected boolean, found {}", el_type),
                        )
                        .emit(db);
                }
            }

            Type::Boolean
        }
        ExpressionNode::NorExpressionNode(node) => {
            let node_list = node.list().unwrap().all_expression_node();
            for element in node_list {
                let el_type = type_check_expression(db, &element, in_node);
                if el_type != Type::Boolean || el_type != Type::Unknown {
                    Diagnostic::new(Level::Error, "Incorrect type")
                        .with_attachment(
                            Span::of_node(db, element.syntax()),
                            format!("expected boolean, found {}", el_type),
                        )
                        .emit(db);
                }
            }

            Type::Boolean
        }
        ExpressionNode::IdentExpressionNode(node) => {
            let ident = some_or_unknown!(some_or_unknown!(node.id_node()).ident());
            let query = NameResolveQuery {
                ident: ident.clone(),
                in_node: in_node.clone(),
            };

            let resolved = declared_type_of_ident(db, query);
            match resolved.as_ref().as_ref() {
                Some(ty) => ty.clone(),
                None => {
                    let name = ident.text();
                    let span = Span::of_token(db, ident.syntax());

                    Diagnostic::new(Level::Error, format!("cannot find value {name:?}"))
                        .with_attachment(span, "not found in this scope")
                        .emit(db);

                    Type::Unknown
                }
            }
        }
        ExpressionNode::ParExpressionNode(node) => {
            type_check_expression(db, &some_or_unknown!(node.expression_node()), in_node)
        }
        ExpressionNode::CallByPosExpressionNode(expr) => {
            let name = expr
                .node_ref()
                .and_then(|r| r.id_node())
                .and_then(|i| i.ident());

            if let Some(name) = name {
                let node_node = crate::name_resolution::find_node(db, name.text().into());

                if let Some(node_node) = Option::clone(&node_node) {
                    let sig = crate::get_typed_signature(db, node_node.clone());
                    return check_call_expression(db, expr, &sig, &Some(node_node));
                } else {
                    let span = Span::of_token(db, name.syntax());

                    Diagnostic::new(Level::Error, format!("unknown node {:?}", name.text()))
                        .with_attachment(span, "not found in this scope")
                        .emit(db);
                }
            }

            Type::Unknown
        }
    }
}

fn check_call_expression(
    db: &Database,
    expr: &CallByPosExpressionNode,
    sig: &TypedSignature,
    in_node: &Option<NodeNode>,
) -> Type {
    // Check input parameters
    let expected = sig.params.iter().map(Some).chain(std::iter::repeat(None));
    let found = expr.args().skip(1).map(Some).chain(std::iter::repeat(None));
    for (expected, found) in expected.zip(found) {
        match (expected, found) {
            (None, None) => break,
            (Some((_, expected_ty)), Some(found)) => {
                if !expected_ty.is_unknown() {
                    let found_ty = type_check_expression(db, &found, in_node);
                    if !found_ty.is_unknown() && expected_ty != &found_ty {
                        let span = Span::of_node(db, found.syntax());
                        Diagnostic::new(Level::Error, "invalid type for argument")
                            .with_attachment(
                                span,
                                format!("expected {expected_ty}, found {found_ty}"),
                            )
                            .emit(db);
                    }
                }
            }
            (Some((expected_ident, _expected_type)), None) => {
                let error_span = expr
                    .args()
                    .last()
                    .map(|s| Span::of_node(db, s.syntax()))
                    .or_else(|| expr.open_par().map(|p| Span::of_token(db, p.syntax())))
                    .or_else(|| expr.node_ref().map(|i| Span::of_node(db, i.syntax())))
                    .unwrap_or_else(|| Span::of_node(db, expr.syntax()))
                    .after();

                let name_span = Span::of_node(db, expr.node_ref().unwrap().syntax());
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
                let name_span = Span::of_node(db, expr.node_ref().unwrap().syntax());
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

fn type_check_left(db: &yeter::Database, expr: &LeftItemNode, in_node: &Option<NodeNode>) -> Type {
    match expr {
        LeftItemNode::IdNode(ident) => {
            let query = NameResolveQuery {
                ident: ident.ident().unwrap().clone(),
                in_node: in_node.clone(),
            };
            let resolved_node = resolve_runtime_node(db, query);
            match *resolved_node {
                Some(ResolvedRuntimeNode::Const(ref const_decl_node)) => {
                    type_of_ast_type(db, in_node.clone(), some_or_unknown!(const_decl_node.type_node())).as_ref().clone()
                }
                Some(ResolvedRuntimeNode::Param(ref var_decl_node))
                | Some(ResolvedRuntimeNode::ReturnParam(ref var_decl_node))
                | Some(ResolvedRuntimeNode::Var(ref var_decl_node)) => {
                    type_of_ast_type(db, in_node.clone(), some_or_unknown!(var_decl_node.type_node())).as_ref().clone()
                }
                None => Type::Unknown,
            }
        }
        LeftItemNode::LeftTableAccessNode(table_item) => {
            type_check_left(db, &table_item.left_item_node().unwrap(), in_node)
        }
        LeftItemNode::LeftFieldAccessNode(_) => {
            todo!("Structures are not supported yet.")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::name_resolution::find_node;

    use super::*;

    #[test]
    fn node_type_check() {
        let mut db = crate::driver();
        crate::add_source_contents(&mut db, String::from("node add() returns (); let tel;
                                                                node sub() returns (); let tel;
                                                                node id(x: int) returns (y: int); let y=x; tel;
        "));

        assert_eq!(
            type_check_query(&db, find_node(&db, "add".into()).as_ref().as_ref().unwrap().clone()).as_ref(),
            &Type::Function {
                args: vec![],
                ret: vec![]
            }
        );
        assert_eq!(
            type_check_query(&db, find_node(&db, "sub".into()).as_ref().as_ref().unwrap().clone()).as_ref(),
            &Type::Function {
                args: vec![],
                ret: vec![]
            }
        );
        assert_eq!(
            type_check_query(&db, find_node(&db, "id".into()).as_ref().as_ref().unwrap().clone()).as_ref(),
            &Type::Function {
                args: vec![Type::Integer],
                ret: vec![Type::Integer]
            }
        );
    }
}
