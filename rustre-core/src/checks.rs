//! Collection of random checks that must be run for a program to be valid
//!
//! All of them should be directly or indirectly called by [`rustre_core::check`][crate::check()]

use crate::{Diagnostic, Level, Span};
use rustre_parser::ast::{AstNode, AstToken, NodeNode, NodeProfileNode, ParamsNode};
use yeter::Database;

/// Checks that the number of params and return params is strictly greater than 0
///
/// For some weird reason, Lustre imposes this restriction, not least in its very grammar. The
/// parser accepts an arity of zero, but the check needs to be done later for spec compliance.
#[yeter::query]
pub fn check_arity(db: &Database, node: NodeNode) {
    let sig = crate::get_signature(db, node.clone());

    let get_span = |f: fn(&NodeProfileNode) -> Option<ParamsNode>| {
        node.node_profile_node()
            .map(|profile| {
                f(&profile)
                    .map(|params| Span::of_node(db, params.syntax()))
                    .unwrap_or_else(|| Span::of_node(db, profile.syntax()))
            })
            .unwrap_or_else(|| Span::of_node(db, node.syntax()))
    };

    if sig.params.is_empty() {
        let name = sig.name.as_ref().map(|i| i.text()).unwrap_or_default();
        let span = get_span(NodeProfileNode::params);

        Diagnostic::new(Level::Error, format!("node {:?} takes no parameter", name))
            .with_attachment(span, "lustre nodes must take at least 1 parameter")
            .emit(db)
    }

    if sig.return_params.is_empty() {
        let name = sig.name.as_ref().map(|i| i.text()).unwrap_or_default();
        let span = get_span(NodeProfileNode::return_params);

        Diagnostic::new(Level::Error, format!("node {:?} returns nothing", name))
            .with_attachment(span, "lustre functions must return at least 1 value")
            .emit(db)
    }
}
