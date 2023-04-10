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

use crate::diagnostics::{Diagnostic, Level, Span};
use rustre_parser::ast::expr_visitor::ExpressionWalker;
use rustre_parser::ast::{
    ArrowExpressionNode, AstToken, CallByPosExpressionNode, ExpressionNode, FbyExpressionNode,
    NodeNode, PreExpressionNode,
};
use std::collections::HashSet;
use yeter::Database;

struct NodeStateWalker<'db> {
    db: &'db Database,
    collected: HashSet<ExpressionNode>,
}

impl<'db> NodeStateWalker<'db> {
    fn push(&mut self, node: ExpressionNode) {
        let present = !self.collected.insert(node);

        debug_assert!(
            !present,
            "attempted to insert expression twice in NodeStateWalker"
        );
    }
}

impl<'db> ExpressionWalker for NodeStateWalker<'db> {
    fn walk_pre(&mut self, e: PreExpressionNode) {
        self.push(ExpressionNode::PreExpressionNode(e));
    }

    fn walk_fby(&mut self, e: FbyExpressionNode) {
        self.push(ExpressionNode::FbyExpressionNode(e));
    }

    fn walk_arrow(&mut self, e: ArrowExpressionNode) {
        self.push(ExpressionNode::ArrowExpressionNode(e));
    }

    fn walk_call_by_pos(&mut self, e: CallByPosExpressionNode) {
        if let Some(node_name) = e
            .node_ref()
            .and_then(|n| n.id_node())
            .and_then(|i| i.ident())
        {
            let sub_node = Option::clone(&crate::name_resolution::find_node(
                self.db,
                node_name.text().into(),
            ));

            if matches!(sub_node, Some(sub_node) if *is_node_stateful(self.db, sub_node.clone())) {
                self.push(ExpressionNode::CallByPosExpressionNode(e));
            }
        }
    }
}

/// **Query:** Returns a list of stateful expressions in a node
#[yeter::query]
pub fn stateful_expr_of_node(db: &Database, node: NodeNode) -> Vec<ExpressionNode> {
    let Some(body) = node.body_node() else {
        return Default::default();
    };

    let mut walker = NodeStateWalker {
        db,
        collected: Default::default(),
    };

    body.all_equals_equation_node()
        .flat_map(|e| e.expression_node())
        .chain(
            body.all_assert_equation_node()
                .flat_map(|e| e.expression_node()),
        )
        .for_each(|e| {
            walker.walk_expr(e);
        });

    walker.collected.into_iter().collect()
}

/// **Query:** Returns `true` if a node contains any stateful expressions (and is thus stateful
/// itself)
#[yeter::query]
pub fn is_node_stateful(db: &Database, node: NodeNode) -> bool {
    !stateful_expr_of_node(db, node).is_empty()
}

/// **Query:** Checks the coherence between the use of the `function` or `node` keyword and the
/// presence or absence of temporal state
#[yeter::query]
pub fn check_node_function_state(db: &Database, node: NodeNode) {
    let has_no_state = !*is_node_stateful(db, node.clone());

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
