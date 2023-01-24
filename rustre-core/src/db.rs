use std::{path::PathBuf, collections::HashMap};

use salsa;

use crate::SourceFile;

/// The salsa database
#[salsa::db(crate::Jar)]
pub struct Database {
    /// The salsa cache
    storage: salsa::Storage<Self>,
    /// Files that are known by the compiler
    files: HashMap<PathBuf, crate::SourceFile>,
}

impl salsa::Database for Database {}

impl Database {
    /// Creates a new compiler database
    pub fn new() -> Self {
        Database { storage: salsa::Storage::default(), files: HashMap::new() }
    }

    /// Adds a source file to the list of files that are known by the compiler
    pub fn add_source_file(&mut self, path: PathBuf) {
        let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
        let file = SourceFile::new(self, path.clone(), contents);
        self.files.insert(path, file);
    }

    /// Iterates over the files that the compiler knows about
    pub fn files(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.values()
    }
}