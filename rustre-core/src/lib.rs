//! Rustre compiler driver

use salsa;
mod db;

#[salsa::jar(db = Db)]
pub struct Jar(
    // inputs
    crate::db::SourceFile,
    // tracked data
    Ast,
    // queries
    parse_file,
);

pub trait Db: salsa::DbWithJar<Jar> {}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

pub fn driver() -> db::Database {
    db::Database::default()
}

// Parsing queries

#[salsa::tracked]
pub struct Ast {
    pub root: rustre_parser::ast::Root,
}

#[salsa::tracked]
pub fn parse_file(db: &dyn crate::Db, file: crate::db::SourceFile) -> Ast {
    let source = file.text(db);
    // TODO: report errors
    let (root, _errors) = rustre_parser::parse(source);
    Ast::new(db, root)
}
