use std::path::PathBuf;

use salsa;

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

impl Database {
    pub fn add_source_file(&self, path: PathBuf) {
        let contents = std::fs::read_to_string(&path).unwrap(); // TODO: report the error
        SourceFile::new(self, path, contents);
    }
}

#[salsa::input]
pub struct SourceFile {
    #[return_ref]
    pub path: PathBuf,
    #[return_ref]
    pub text: String,
}
