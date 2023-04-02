#![feature(try_trait_v2)]

//! Rustre compiler driver
//!
//! It is built around [yeter].

use std::path::PathBuf;
use crate::diagnostics::{Diagnostic, Level, Span};

use rustre_parser::ast::{
    AstNode, Ident, NodeNode, NodeProfileNode, ParamsNode, Root, TypedIdsNode,
};
use std::rc::Rc;
use yeter::Database;

pub mod diagnostics;
pub mod expression;
pub mod name_resolution;
pub mod node_state;
pub mod eval;
mod types;

/// Builds a new compiler driver, that corresponds to a compilation session
pub fn driver() -> Database {
    Database::new()
}

// Inputs
// TODO: maybe they should be moved to their own module

#[derive(Clone, Hash)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

impl SourceFile {
    pub fn new(path: PathBuf, text: String) -> SourceFile {
        SourceFile { path, text }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct Signature {
    pub name: Option<Ident>,
    pub params: Vec<TypedIdsNode>,
    pub return_params: Vec<TypedIdsNode>,
}

#[derive(Clone, Debug, Hash)]
pub struct TypedSignature {
    pub name: Option<Ident>,
    pub params: Vec<(Ident, types::Type)>,
    pub return_params: Vec<(Ident, types::Type)>,
}

/// **Query**: Parses a given file
#[yeter::query]
pub fn parse_file(db: &Database, file: SourceFile) -> Root {
    let source = file.text;

    let (root, errors) = rustre_parser::parse(&source);
    for error in errors {
        let span = Span {
            file: file.path.clone(),
            start: error.span.start,
            end: error.span.end,
        };

        Diagnostic::new(Level::Error, "parsing error")
            .with_attachment(span, error.msg)
            .emit(db);
    }

    db.set::<diagnostics::file_for_root>((root.syntax().clone(),), Some(file.path));

    root
}

/// **Query**: Returns a list of all directly and indirectly included files in the Lustre program
#[yeter::query]
pub fn files(_db: &Database) -> Option<Vec<SourceFile>>;

#[yeter::query]
fn parsed_files(db: &Database) -> Vec<Rc<Root>> {
    let files = files(db);
    if let Some(files) = files.as_ref() {
        files
            .iter()
            .map(|s| parse_file(db, s.clone()))
            .collect::<Vec<_>>()
    } else {
        vec![]
    }
}

#[yeter::query]
pub fn get_signature(_db: &Database, node: NodeNode) -> Signature {
    println!("coucou c la signgature:!!!!!\n\n\n");
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

#[yeter::query]
pub fn get_typed_signature(db: &Database, node: NodeNode) -> TypedSignature {
    let sig = get_signature(db, node);

    let get_params = |params: &[TypedIdsNode]| {
        params.iter().flat_map(|group| {
            let ty = group.type_node()
                .map(|t| types::type_of_ast_type(db, None, t))
                .unwrap_or_default();

            group.all_ident().zip(std::iter::repeat(ty.as_ref().clone()))
        }).collect::<Vec<_>>()
    };

    TypedSignature {
        name: sig.name.clone(),
        params: get_params(&sig.params),
        return_params: get_params(&sig.return_params),
    }
}

/// **Query:** Global program check
#[yeter::query]
pub fn check(db: &Database) {
    let files = parsed_files(db);
    for file in files.as_slice() {
        for node in file.all_node_node() {
            let _ = get_typed_signature(db, node.clone());

            if let Some(body) = node.clone().body_node() {
                for equation in body.all_equals_equation_node() {
                    if let Some(expression) = equation.expression_node() {
                        let _ = types::type_check_expression(db, &expression, &Some(node.clone()));
                    }
                }
            }

            node_state::check_node_function_state(db, node);
        }
    }
}

/// Adds a source file to the list of files that are known by the compiler
pub fn add_source_file(db: &Database, path: PathBuf) {
    let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
    let file = SourceFile::new(path, contents);
    let files = files(db);
    let mut files = Option::clone(&files).unwrap_or_default();
    files.push(file);
    db.set::<files>((), Some(files));
}

pub fn add_source_contents(db: &mut Database, contents: String) {
    let file = SourceFile::new(PathBuf::new(), contents);
    let files = files(db);
    let mut files = Option::clone(&files).unwrap_or_default();
    files.push(file);
    db.set::<files>((), Some(files));
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn parse_query() {
        let driver = super::driver();
        super::add_source_file(&driver, Path::new("../tests/stable.lus").to_owned());
        let files = super::files(&driver);
        let files = files.as_ref().as_deref().unwrap_or_default();
        for file in files {
            let ast = super::parse_file(&driver, file.clone());
            assert_eq!(ast.all_include_statement().count(), 1);
        }
    }
}
