pub mod ast;
pub mod lexer;
pub mod location;
pub mod parser;

use lexer::LustreLang;
use logos::Logos;

type Error = String;

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
        let mut tokens: Vec<_> = lexer.map(|(tok, span)| (tok, &src[span])).collect();
        tokens.reverse();
        parser::Parser::parse(tokens)
    }
}
