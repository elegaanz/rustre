pub mod ast;
pub mod lexer;
pub mod location;
pub mod parser;
mod rowan_nom;

use std::ops::Range;

use crate::rowan_nom::RowanNomError;
use lexer::LustreLang;
use logos::Logos;

/// TODO remove `Debug`
#[derive(Debug)]
pub struct Error {
    pub span: Range<usize>,
    pub msg: String,
}

impl RowanNomError for Error {
    fn from_message(message: &str) -> Self {
        Error {
            span: 0..0,
            msg: message.to_string(),
        }
    }
}

pub type SyntaxNode = rowan::SyntaxNode<LustreLang>;
pub type SyntaxToken = rowan::SyntaxToken<LustreLang>;

/// A Lustre v6 parser
pub struct Parse {
    pub root: SyntaxNode,
    pub errors: Vec<Error>,
}

impl Parse {
    pub fn parse(src: &str) -> Self {
        let lexer = lexer::Token::lexer(src).spanned();
        let tokens: Vec<_> = lexer.map(|(tok, span)| (tok, &src[span])).collect();
        parser::Parser::parse(tokens)
    }
}
