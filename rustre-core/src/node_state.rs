//! Persisted node state (temporal operators)
//!
//! In Lustre, nodes may use different operators to use the language's temporal features, namely
//! `pre`, `->` and `fby`.
//! While `function` nodes behave mostly like traditional functions, actual `node`s need to persist
//! data from an invocation to the next in order to implement the above operators correctly.
//!
//! This module defines queries to obtain the state that a given node requires, that is, a list of
//! typed "slots" of memory that a Rustre runtime would need to allocate for each instance of the
//! aforementioned node.
//! Additionally, it provides a [check_node_function_state][check_node_function_state()] query to
//! emit diagnostics as to whether or not a given node is correctly defined as `function` or `node`,
//! if it has, respectively no, or some state.
//!
//! # Stateful expressions
//!
//! 4 kinds of Lustre expression require persisted memory between node invocations.
//!
//!   * The `pre` unary operator: evaluates to the previous value it was applied to. It requires a
//!     (nullable) "slot" of the same type as its operand.
//!   * The `fby` binary operator: evaluates to the value of its first operand the first cycle, and
//!     then to the previous value of its second operand for the remaining ones. It also requires a
//!     (nullable) slot with the same type as its operands.
//!   * The `->` binary operator evaluates to the value of its first operand the first cycle, and
//!     then to the value of its second operand for the remaining ones. It only requires a boolean
//!     value to be represented as it doesn't persist any Lustre data; it must just know if it is
//!     in its first evaluation cycle.
//!   * Node call sites: each call site corresponds to an _instanciation_ of a node, with its own
//!     memory. They have to be recursively accounted for.

use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use yeter::Database;

use crate::diagnostics::{Diagnostic, Level, Span};
use rustre_parser::ast::{AstToken, CallByPosExpressionNode, ExpressionNode, NodeNode};

use crate::types::Type;

/// Description of the temporally persisted data of a given node
///
/// The indices of entries in both [BTreeMap]s is considered stable and may be used for
/// identification.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct NodeState {
    /// Mapping of call site expressions to the [NodeState] of the resolved node
    pub call_sites: BTreeMap<CallByPosExpressionNode, Rc<NodeState>>,

    /// Mapping of a temporal [ExpressionNode] to the type that needs to be stored
    ///
    /// Its keys may only correspond to one of the 3 temporal operators.
    pub operators: BTreeMap<ExpressionNode, Type>,
}

impl NodeState {
    /// Returns `true` if the node associated to this [NodeState] doesn't hold any state
    pub fn is_empty(&self) -> bool {
        self.call_sites.values().all(|s| s.is_empty()) && self.operators.is_empty()
    }

    fn push_call_site(&mut self, call_site: CallByPosExpressionNode, sub_state: Rc<NodeState>) {
        let present = self.call_sites.insert(call_site, sub_state).is_some();

        debug_assert!(!present, "attempted to insert call site twice in NodeState");
    }

    fn push_operator(&mut self, node: ExpressionNode, ty: Type) {
        let present = self.operators.insert(node, ty).is_some();

        debug_assert!(
            !present,
            "attempted to insert expression twice in NodeState"
        );
    }
}

macro_rules! e {
    ($stack:ident += $o:expr) => {{
        $stack.extend($o.operand());
    }};
    ($stack:ident ++= $o:expr) => {{
        $stack.extend($o.left());
        $stack.extend($o.right());
    }};
}

fn extract_state(
    db: &Database,
    expr: ExpressionNode,
    stack: &mut impl Extend<ExpressionNode>,
    builder: &mut NodeState,
    in_node: &Option<NodeNode>,
) {
    match &expr {
        ExpressionNode::PreExpressionNode(e) => {
            let Some(operand) = e.operand() else {
                return;
            };

            let ty = crate::types::type_check_expression(db, &operand, in_node).ok();
            stack.extend([operand]);

            if let Some(ty) = ty {
                builder.push_operator(expr, ty);
            }
        }
        ExpressionNode::FbyExpressionNode(e) => {
            stack.extend(e.left());
            stack.extend(e.right());

            let Some(left) = e.left() else {
                return;
            };

            let ty = crate::types::type_check_expression(db, &left, in_node).ok();

            if let Some(ty) = ty {
                builder.push_operator(expr, ty);
            }
        }
        ExpressionNode::ArrowExpressionNode(e) => {
            stack.extend(e.left());
            stack.extend(e.right());
            builder.push_operator(expr, Type::Boolean);
        }

        ExpressionNode::ConstantNode(_) => (),
        ExpressionNode::IdentExpressionNode(_) => (),
        ExpressionNode::NotExpressionNode(e) => e!(stack += e),
        ExpressionNode::NegExpressionNode(e) => e!(stack += e),
        ExpressionNode::CurrentExpressionNode(e) => e!(stack += e),
        ExpressionNode::IntExpressionNode(e) => e!(stack += e),
        ExpressionNode::RealExpressionNode(e) => e!(stack += e),
        ExpressionNode::WhenExpressionNode(e) => stack.extend(e.left()),
        ExpressionNode::AndExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::OrExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::XorExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::ImplExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::EqExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::NeqExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::LtExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::LteExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::GtExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::GteExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::DivExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::ModExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::SubExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::AddExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::MulExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::PowerExpressionNode(e) => e!(stack ++= e),
        ExpressionNode::IfExpressionNode(e) => {
            stack.extend(e.cond());
            stack.extend(e.if_body());
            stack.extend(e.else_body());
        },
        ExpressionNode::WithExpressionNode(e) => {
            stack.extend(e.cond());
            stack.extend(e.with_body());
            stack.extend(e.else_body());
        },
        ExpressionNode::DieseExpressionNode(e) => {
            stack.extend(e.list().iter().flat_map(|el| el.all_expression_node()));
        },
        ExpressionNode::NorExpressionNode(e) => {
            stack.extend(e.list().iter().flat_map(|el| el.all_expression_node()));
        },
        ExpressionNode::ParExpressionNode(e) => {
            stack.extend(e.expression_node());
        }
        ExpressionNode::CallByPosExpressionNode(e) => {
            if let Some(node_name) = e
                .node_ref()
                .and_then(|n| n.id_node())
                .and_then(|i| i.ident())
            {
                let sub_node = crate::name_resolution::find_node(db, node_name.text().into());
                let sub_state = Option::clone(&sub_node).map(|n| state_of(db, n));
                builder.push_call_site(e.clone(), sub_state.unwrap_or_default());
            }

            stack.extend(e.args());
        }
    }
}

/// **Query:** Returns a list of types that a node needs to persist from a call to the next
#[yeter::query]
pub fn state_of(db: &Database, node: NodeNode) -> NodeState {
    let Some(body) = node.body_node() else {
        return NodeState::default();
    };

    let all_expressions = body
        .all_equals_equation_node()
        .flat_map(|e| e.expression_node())
        .chain(
            body.all_assert_equation_node()
                .flat_map(|e| e.expression_node()),
        );

    let mut state = NodeState::default();

    // Recurse on a tree of expressions and collect any signs of state
    let mut stack = all_expressions.collect::<Vec<_>>();
    while let Some(expr) = stack.pop() {
        extract_state(db, expr, &mut stack, &mut state, &Some(node.clone()));
    }

    state
}

/// **Query:** Checks the coherence between the use of the `function` or `node` keyword and the
/// presence or absence of temporal state
#[yeter::query]
pub fn check_node_function_state(db: &Database, node: NodeNode) {
    let has_no_state = state_of(db, node.clone()).is_empty();

    if node.is_node() && has_no_state {
        let span = Span::of_token(db, node.node().unwrap().syntax());

        Diagnostic::new(
            Level::Warning,
            "node has no internal state, it should be declared as `function`",
        )
        .with_attachment(span, "hint: replace with `function`")
        .emit(db);
    }

    if node.is_function() && !has_no_state {
        let span = Span::of_token(db, node.function().unwrap().syntax());

        Diagnostic::new(
            Level::Error,
            "function has internal state, it should be a node",
        )
        .with_attachment(span, "hint: replace with `node`")
        .emit(db);
    }
}
