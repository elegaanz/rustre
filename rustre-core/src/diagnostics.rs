use std::fmt::{Debug, Formatter};
use std::path::{Path, PathBuf};

use rustre_parser::{SyntaxElement, SyntaxNode, SyntaxToken};

#[derive(Clone)]
pub struct Span {
    pub file: PathBuf,
    pub start: usize,
    pub end: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{:?}", self.file.display(), self.start..self.end)
    }
}

#[yeter::query]
pub fn file_for_root(db: &yeter::Database, _root: SyntaxNode) -> Option<PathBuf> {
    let files = crate::files(db);
    let files = files.as_ref().as_deref().unwrap_or_default();

    // TODO: we assume only one file is loaded, that's stupid
    files.get(0).map(|s| s.path.clone())
}

/// Returns the length of the trivia preceding a node
fn preceding_trivia_len(syntax: &SyntaxNode) -> usize {
    syntax
        .children_with_tokens()
        .map_while(|el| match el {
            SyntaxElement::Token(t) if !t.kind().is_trivia() => None,
            SyntaxElement::Token(t) => Some(t.text().len()),
            SyntaxElement::Node(n) => Some(preceding_trivia_len(&n)),
        })
        .sum()
}

impl Span {
    pub fn of_token(db: &yeter::Database, syntax_token: &SyntaxToken) -> Self {
        let range = syntax_token.text_range();
        let root = syntax_token.parent_ancestors().last().unwrap();
        let file = Option::clone(&file_for_root(db, root)).expect("AST not bound to a file");
        Span {
            file,
            start: range.start().into(),
            end: range.end().into(),
        }
    }

    pub fn of_node(db: &yeter::Database, syntax_node: &SyntaxNode) -> Self {
        let to_skip = preceding_trivia_len(syntax_node);
        let range = syntax_node.text_range();
        let root = syntax_node
            .ancestors()
            .last()
            .unwrap_or(syntax_node.clone());

        let file = Option::clone(&file_for_root(db, root)).expect("AST not bound to a file");
        Span {
            file,
            start: to_skip + usize::from(range.start()),
            end: range.end().into(),
        }
    }

    /// Returns a 0-long span located just after another span, useful for reported missing syntax
    pub fn after(mut self) -> Self {
        self.start = self.end;
        self
    }
}

#[derive(Clone, Debug)]
#[must_use]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub attachments: Vec<(Span, String)>,
}

impl Diagnostic {
    pub fn new(level: Level, message: impl Into<String>) -> Self {
        Self {
            level,
            message: message.into(),
            attachments: vec![],
        }
    }

    pub fn with_attachment(mut self, span: Span, message: impl Into<String>) -> Self {
        self.attachments.push((span, message.into()));
        self
    }

    pub fn file_context(&self) -> Option<(&Path, usize)> {
        self.attachments
            .first()
            .map(|(span, _)| (span.file.as_path(), span.start))
    }

    pub fn emit(self, db: &yeter::Database) {
        db.do_effect(self);
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Level {
    Debug,
    Info,
    Warning,
    Error,
}
