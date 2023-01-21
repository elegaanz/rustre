//! Structures and enums to represent Lustre's syntax tree
//!
//! # Internal representation
//!
//! The structures in this module are all merely wrappers around
//! [rowan's `SyntaxNode`][rowan::SyntaxNode] that provide useful specific getters for each node
//! kind.

mod generated;

use crate::lexer::Token;
use crate::{SyntaxNode, SyntaxToken};
#[doc(inline)]
pub use generated::*;

// These two traits have been stolen from rust-analyzer
// (and a lot of other things in this crate is actually inspired by RA)

pub trait AstNode {
    fn can_cast(kind: Token) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;
    fn expect(syntax: SyntaxNode) -> Self
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

pub trait AstToken {
    fn can_cast(token: Token) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn expect(syntax: SyntaxToken) -> Self
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}
