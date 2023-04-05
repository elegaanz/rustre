#![cfg(test)]

use crate::ast::{AstToken, Root};

fn parse(source: &str) -> Root {
    crate::parse(source).0
}

#[test]
fn enumerate_nodes_invalid_syntax() {
    let root = parse(
        "node add() returns (); let tel;
node sub() returns ();",
    );

    let nodes = root.all_node_node().collect::<Vec<_>>();
    assert_eq!(nodes.len(), 2);
    assert_eq!(nodes[0].id_node().unwrap().ident().unwrap().text(), "add");
    assert_eq!(nodes[1].id_node().unwrap().ident().unwrap().text(), "sub");
}

#[test]
fn node_params() {
    let root = parse("node add(a, b : bool; c : bool) returns (r, c : bool); let tel;");

    let node = root.all_node_node().next().expect("no node");
    let node_profile = node.node_profile_node().unwrap();
    let params = node_profile.params().unwrap();
    let returns = node_profile.return_params().unwrap();

    let params = params.all_var_decl_node().collect::<Vec<_>>();
    assert_eq!(params.len(), 2);
    assert!(params[0]
        .all_typed_ids_node()
        .next()
        .unwrap()
        .type_node()
        .unwrap()
        .bool()
        .is_some());

    let returns = returns.all_var_decl_node().collect::<Vec<_>>();
    assert_eq!(returns.len(), 1);
    let returns_names = returns[0]
        .all_typed_ids_node()
        .next()
        .unwrap()
        .all_ident()
        .collect::<Vec<_>>();

    assert_eq!(returns_names[0].text(), "r");
    assert_eq!(returns_names[1].text(), "c");
}

#[test]
fn labeled_expression_are_different() {
    let root = parse("node add(a, b : int) returns (c : int); let c = a + b; tel;");
    dbg!(&root);
    let node = root.all_node_node().next().unwrap();
    let body = node.body_node().unwrap();
    let eq = body.all_equals_equation_node().next().unwrap();
    let rhs = eq.expression_node().unwrap();
    let addition = rhs.unwrap_add_expression_node();
    let left = addition.left().unwrap();
    let right = addition.right().unwrap();
    assert!(left != right);
}
