//! Rustre compiler driver
//!
//! It is built around [yeter].

use std::path::PathBuf;

use yeter;

pub mod expression;
pub mod name_resolution;
pub mod node_graph;
mod types;

use node_graph::{NodeGraph, NodeGraphBuilder};
use rustre_parser::ast::{Ident, NodeNode, NodeProfileNode, ParamsNode, Root, TypedIdsNode, AstToken};
use std::rc::Rc;
use yeter::Database;

/// Builds a new compiler driver, that corresponds to a compilation session.
///
/// This function should only be called once.
pub fn driver() -> Database {
    let mut db = Database::new();
    db.register_impl::<parse_file>();
    db.register::<_, files>(|_db, ()| vec![]);
    db.register_impl::<build_node_graph>();
    db.register_impl::<types::type_check_query>();
    db.register::<_, find_node>(|db, (node_name,)| {
        for file in &*files(db) {
            let ast = parse_file(db, file.clone());
            for node in ast.all_node_node() {
                if node.id_node()?.ident()?.text() == node_name {
                    return Some(node.clone());
                }
            }
        }
        None
    });

    db.register::<_, files>(|_db, ()| vec![]);
    db.register_impl::<build_node_graph>();
    db.register_impl::<parsed_files>();

    db.register_impl::<get_signature>();

    // mod name_resolution
    db.register_impl::<name_resolution::resolve_const_node>();
    db.register_impl::<name_resolution::resolve_const_expr_node>();
    db.register_impl::<name_resolution::resolve_runtime_node>();
    db.register_impl::<name_resolution::resolve_runtime_expr_node>();

    db
}

#[yeter::query]
fn find_node(db: &Database, node_name: String) -> Option<NodeNode>;

// Inputs
// TODO: maybe they should be moved to their own module

#[derive(Clone, Hash)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

impl SourceFile {
    fn new(path: PathBuf, text: String) -> SourceFile {
        SourceFile { path, text }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct Signature {
    name: Option<Ident>,
    params: Vec<TypedIdsNode>,
    return_params: Vec<TypedIdsNode>,
}

/// **Query**: Parses a given file
#[yeter::query]
pub fn parse_file(_db: &Database, file: SourceFile) -> Root {
    let source = file.text;
    // TODO: report errors
    let (root, _errors) = rustre_parser::parse(&source);
    root
}

/// **Query**: Returns a list of all directly and indirectly included files in the Lustre program
#[yeter::query]
pub fn files(_db: &Database) -> Vec<SourceFile>;

#[yeter::query]
fn parsed_files(db: &Database) -> Vec<Rc<Root>> {
    files(db)
        .iter()
        .map(|s| parse_file(db, s.clone()))
        .collect::<Vec<_>>()
}

#[yeter::query]
pub fn build_node_graph(_db: &Database, node: NodeNode) -> NodeGraph {
    let mut builder = NodeGraphBuilder::default();
    let graph = builder.try_parse_node_graph(&node);

    if !builder.errors.is_empty() {
        // TODO: report errors
        eprint!(
            "yeter doesn't support error reporting but we got these: {:?}",
            &builder.errors
        );
    }

    graph
}

#[yeter::query]
pub fn get_signature(_db: &Database, node: NodeNode) -> Signature {
    let sig = node.node_profile_node();

    let get_params = |f: fn(&NodeProfileNode) -> Option<ParamsNode>| {
        sig.clone()
            .and_then(|sig| f(&sig))
            .and_then(|p| p.all_var_decl_node().next())
            .iter()
            .flat_map(|v| v.all_typed_ids_node())
            .collect::<Vec<_>>()
    };

    Signature {
        name: node.id_node().and_then(|id| id.ident()),
        params: get_params(NodeProfileNode::params),
        return_params: get_params(NodeProfileNode::return_params),
    }
}

/// Adds a source file to the list of files that are known by the compiler
pub fn add_source_file(db: &mut Database, path: PathBuf) {
    let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
    let file = SourceFile::new(path, contents);
    let files = files(db);
    let mut files = (*files).clone();
    files.push(file);
    db.register::<_, files>(move |_db, ()| {
        files.clone() // TODO: find a way to not clone?
    })
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn parse_query() {
        let mut driver = super::driver();
        super::add_source_file(&mut driver, Path::new("../tests/stable.lus").to_owned());
        for file in &*super::files(&driver) {
            let ast = super::parse_file(&driver, file.clone());
            assert_eq!(ast.all_include_statement().count(), 1);
        }
    }
}
