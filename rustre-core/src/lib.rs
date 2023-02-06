//! Rustre compiler driver
//!
//! It is built around [salsa].

use std::path::PathBuf;

use rustre_parser::ast::Root;
use yeter;

mod types;

/// Builds a new compiler driver, that corresponds to a compilation session.
///
/// This function should only be called once.
pub fn driver() -> yeter::Database {
    let db = yeter::Database::new();
    db.register::<parse::parse_file::Query>(|db, file| {
        let source = file.text;
        // TODO: report errors
        let (root, _errors) = rustre_parser::parse(&source);
        root
    });
    db.register::<files::files::Query>(|db, ()| {
        vec![]
    }); 
    db
}

yeter::queries! {
    parse {
        parse_file: crate::SourceFile: rustre_parser::ast::Root
    },
    files {
        files: (): Vec<crate::SourceFile>
    }
}

// Inputs
// TODO: maybe they should be moved to their own module

#[derive(Hash)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
}

impl SourceFile {
    fn new(path: PathBuf, text: String) -> SourceFile {
        SourceFile { path: path, text: text }
    }
}

/// **Query**: Parses a given file
pub fn parse_file(_db: &yeter::Database, file: SourceFile) -> Root {
    let source = file.text;
    // TODO: report errors
    let (root, _errors) = rustre_parser::parse(&source);
    root
}

/// **Query**: Adds a source file to the list of files that are known by the compiler
pub fn add_source_file(db: &yeter::Database, path: PathBuf) {
    let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
    let file = SourceFile::new(path.clone(), contents);
    db.files(file)
}

pub fn files(db: &yeter::Database) -> impl Iterator<Item = &SourceFile> {
    db.register(db, )
}



#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn parse_query() {
        let mut driver = super::driver();
        driver.add_source_file(Path::new("../tests/stable.lus").to_owned());
        for file in driver.files() {
            let ast = super::parse_file(*file);
            assert_eq!(root.all_include_statement().count(), 1);
        }
    }
}
