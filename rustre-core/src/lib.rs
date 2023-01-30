//! Rustre compiler driver
//!
//! It is built around [salsa].
//!
//! [salsa]: https://salsa-rs.github.io/salsa/

use std::path::PathBuf;

use salsa;
mod db;

/// This structure is used to give Salsa all the information
/// it needs to build our query system
#[salsa::jar(db = Db)]
pub struct Jar(
    // inputs
    SourceFile,
    // tracked data
    Ast,
    // queries
    parse_file,
);

/// The database trait
///
/// The salsa database (= cache) is never passed around as a concrete type
/// but is always a `&dyn Db`. This allows the same database to be shared between
/// two different jars.
pub trait Db: salsa::DbWithJar<Jar> {}

/// Auto-impl of the database trait for all concrete databases types
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

/// Builds a new compiler driver, that corresponds to a compilation session.
///
/// This function should only be called once.
pub fn driver() -> db::Database {
    db::Database::new()
}

// Inputs
// TODO: maybe they should be moved to their own module

#[salsa::input]
pub struct SourceFile {
    #[return_ref]
    pub path: PathBuf,
    #[return_ref]
    pub text: String,
}

// Parsing queries
// TODO: maybe they should be moved to their own module

/// Wrapper type to be able to store an AST in the DB
#[salsa::tracked]
pub struct Ast {
    #[return_ref]
    pub root: rustre_parser::ast::Root,
}

/// **Query**: parses a given file
#[salsa::tracked]
pub fn parse_file(db: &dyn crate::Db, file: SourceFile) -> Ast {
    let source = file.text(db);
    // TODO: report errors
    let (root, _errors) = rustre_parser::parse(source);
    Ast::new(db, root)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn parse_query() {
        let mut driver = super::driver();
        driver.add_source_file(Path::new("../tests/stable.lus").to_owned());
        for file in driver.files() {
            let ast = super::parse_file(&driver, *file);
            let root = ast.root(&driver);
            assert_eq!(root.all_include_statement().count(), 1);
        }
    }
}
