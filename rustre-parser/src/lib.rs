//! `rustre_parser` is a Rust implementation of a Lustre parser with the ultimate goal of being
//! entirely compliant with its official [specification][spec] and [OCaml implementation][impl].
//!
//! [spec]: https://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lv6-ref-man.pdf
//! [impl]: https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/

pub mod ast;
mod ast_tests;
pub mod lexer;
pub mod parser;

use crate::lexer::{Lexer, Token};
use rowan_nom::RowanNomError;
use std::ops::Range;

/// Marker, non-constructible type that implements [`rowan::Language`] for Lustre
#[derive(Hash, Ord, PartialOrd, PartialEq, Eq, Debug, Copy, Clone)]
pub enum LustreLang {}

pub struct ParserError {
    pub span: Range<usize>,
    pub msg: String,
    pub cause: Option<Box<ParserError>>,
}

impl RowanNomError<LustreLang> for ParserError {
    fn from_message(message: &str) -> Self {
        ParserError {
            span: 0..0,
            msg: message.to_string(),
            cause: None,
        }
    }

    fn from_expected(position: usize, message: &str) -> Self {
        Self {
            span: position..position,
            msg: message.to_string(),
            cause: None,
        }
    }

    fn from_expected_eof(range: Range<usize>) -> Self {
        Self {
            span: range,
            msg: "expected eof, found token".to_string(),
            cause: None,
        }
    }

    fn from_unexpected_eof(position: usize) -> Self {
        Self {
            span: position..position,
            msg: "unexpected eof".to_string(),
            cause: None,
        }
    }

    fn from_unexpected_token(span: Range<usize>, expected: Token, found: Token) -> Self {
        Self {
            span,
            msg: format!("expected {expected:?}, found {found:?}"),
            cause: None,
        }
    }

    fn with_context(self, ctx: &'static str) -> Self {
        Self {
            span: self.span.clone(),
            msg: ctx.to_string(),
            cause: Some(Box::new(self)),
        }
    }
}

/// [rowan's `SyntaxNode`][rowan::SyntaxNode] parameterized with the Lustre language marker,
/// [`LustreLang`]
pub type SyntaxNode = rowan::SyntaxNode<LustreLang>;

/// [rowan's `SyntaxToken`][rowan::SyntaxToken] parameterized with the Lustre language marker,
/// [`LustreLang`]
pub type SyntaxToken = rowan::SyntaxToken<LustreLang>;

/// Lex a Lustre file into localized [Token]s
///
/// Shorthand for [`Lexer::from_source`].
///
/// # Example
///
/// ```
/// # use rustre_parser::{lex, lexer::Token};
/// // This iterator contains the tokens and their respective range
/// let spanned_tokens = lex("type register = bool^32;");
///
/// // We only keep the tokens and filter out trivia (spaces, comments)
/// let tokens = spanned_tokens.map(|(token, _range)| token).filter(|t| t.is_non_trivia());
///
/// assert_eq!(tokens.collect::<Vec<_>>(), [
///     Token::Type,
///     Token::Ident,
///     Token::Equal,
///     Token::Bool,
///     Token::Hat,
///     Token::IConst,
///     Token::Semicolon,
/// ]);
/// ```
pub fn lex(source: &str) -> Lexer {
    Lexer::from_source(source)
}

/// Parse a Lustre file's source code into a syntax tree and a list of parsing errors
///
/// The syntax tree elements correspond to structs in the [`ast`] module.
///
/// # Errors
///
/// Thanks to the design of the parser, errors don't cause the entire parser to fail, but are rather
/// accumulated in the list while the parser continues its jobs, doing the best it can to land back
/// on its feet. For this reason, errors are returned in a tuple rather than with the usual
/// [Result].
///
/// # Parsing individual grammar elements
///
/// This function parses an entire Lustre program, that is, an entire file. If you want to parse a
/// specific syntax element (say, just a node), you'll have to find the specific parser in the
/// [`parser`] module and build one of [`ast`]'s structs from its result.
///
/// While — as explained above — parsing an entire program isn't supposed to make the actual parser
/// fail, if you go about parsing individual syntax elements, their parsers may fail, typically if
/// the first (few) token(s) are/is unexpected.
pub fn parse(source: &str) -> (ast::Root, Vec<ParserError>) {
    let lexer = Lexer::from_source(source);
    let tokens = lexer
        .map(|(tok, span)| (tok, &source[span]))
        .collect::<Vec<_>>();
    let input = rowan_nom::Input::from(tokens.as_slice());

    match parser::parse_program(input) {
        Ok((_, (root, errors))) => (ast::Root { syntax: root }, errors),
        // Thanks to its design, the parser should never error at the top-level. We handle it still,
        // just in case.
        Err(_) => (
            ast::Root {
                syntax: SyntaxNode::new_root(rowan::GreenNode::new(Token::Root.into(), [])),
            },
            vec![ParserError {
                msg: "unexpected internal parser error; this should never happen".to_string(),
                span: source.len()..source.len(),
                cause: None,
            }],
        ),
    }
}

#[cfg(all(test, feature = "tests-lustre-upstream"))]
rustre_parser_tests_codegen::include_lustre_tests!(mod parser_tests);
