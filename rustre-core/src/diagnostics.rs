use std::path::{PathBuf};

use rustre_parser::{ast::AstNode, SyntaxNode};

#[derive(Clone)]
pub struct Span {
    pub file: PathBuf,
    pub start: usize,
    pub end: usize,
}

#[yeter::query]
pub fn file_for_root(db: yeter::Database, root: SyntaxNode) -> Option<PathBuf>;

impl Span {
    fn of(db: &yeter::Database, syntax_node: &SyntaxNode) -> Self {
        let range = syntax_node.text_range();
        let root = syntax_node.ancestors().last().unwrap_or(syntax_node.clone());
        let file = Option::clone(&file_for_root(db, root)).expect("AST not bound to a file");
        Span {
            file,
            start: range.start().into(),
            end: range.end().into(),
        }
    }
}

#[derive(Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
    pub level: Level,
}

#[derive(Clone)]
pub enum Level {
    Debug,
    Info,
    Warning,
    Error,
}

pub fn emit(db: yeter::Database, node: impl AstNode, message: impl ToString, level: Level) {
    db.do_effect(Diagnostic {
        span: Span::of(&db, node.syntax()),
        message: message.to_string(),
        level,
    })
}
