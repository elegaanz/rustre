//! Rustre compiler driver
//!
//! It is built around [salsa].

pub mod expression;
pub mod node_graph;
mod types;

use node_graph::{NodeGraph, NodeGraphBuilder};
use rustre_parser::ast::{NodeNode, Root};
use std::path::PathBuf;

/// Builds a new compiler driver, that corresponds to a compilation session.
///
/// This function should only be called once.
pub fn driver() -> yeter::Database {
    let mut db = yeter::Database::new();
    db.register::<_, parse::parse_file::Query>(|_db, file| {
        let source = file.text;
        // TODO: report errors
        let (root, _errors) = rustre_parser::parse(&source);
        root
    });
    db.register::<_, files::files::Query>(|_db, ()| vec![]);
    db.register::<_, graph::build_node_graph::Query>(|_db, node| {
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
    });
    db
}

yeter::queries! {
    parse {
        parse_file: crate::SourceFile: rustre_parser::ast::Root
    },
    files {
        files: (): Vec<crate::SourceFile>
    },
    graph {
        build_node_graph: rustre_parser::ast::NodeNode: crate::node_graph::NodeGraph
    }
}

// Inputs
// TODO: maybe they should be moved to their own module

#[derive(Clone, Hash)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

impl SourceFile {
    fn new(path: PathBuf, text: String) -> SourceFile {
        SourceFile {
            path: path,
            text: text,
        }
    }
}

/// **Query**: Parses a given file
pub fn parse_file(_db: &yeter::Database, file: &SourceFile) -> Root {
    let source = &file.text;
    // TODO: report errors
    let (root, _errors) = rustre_parser::parse(source);
    root
}

/// **Query**: Adds a source file to the list of files that are known by the compiler
pub fn add_source_file(db: &mut yeter::Database, path: PathBuf) {
    let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
    let file = SourceFile::new(path.clone(), contents);
    let files = files::files::query(db, ());
    let mut files = (*files).clone();
    files.push(file);
    db.register::<_, files::files::Query>(move |_db, ()| {
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
        for file in &*super::files::files::query(&driver, ()) {
            let ast = super::parse_file(&driver, file);
            assert_eq!(ast.all_include_statement().count(), 1);
        }
    }
}
