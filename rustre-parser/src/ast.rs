//! Structures and enums to represent Lustre's syntax tree
//!
//! # Internal representation
//!
//! The structures in this module are all merely wrappers around
//! [rowan's `SyntaxNode`][rowan::SyntaxNode] that provide useful specific getters for each node
//! kind.

#[allow(warnings, unused)]
mod generated {
    include!(concat!(env!("OUT_DIR"), "/ast_generated.rs"));
}

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

fn debug_ast_node<N: AstNode>(
    node: &N,
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
) -> std::fmt::Result {
    write!(f, "{}@{:?}", name, node.syntax().text_range())
}

// Additional methods that the build script can't generate

impl NodeProfileNode {
    pub fn params(&self) -> Option<ParamsNode> {
        self.syntax
            .children()
            .next()
            .filter(|s| s.kind() == Token::ParamsNode)
            .map(|syntax| ParamsNode { syntax })
    }

    pub fn return_params(&self) -> Option<ParamsNode> {
        self.syntax
            .children_with_tokens()
            .skip_while(|s| s.kind() != Token::Returns)
            .find(|s| s.kind() == Token::ParamsNode)
            .map(|syntax| ParamsNode {
                syntax: syntax.into_node().unwrap(),
            })
    }
}

pub trait BinaryExpression {
    fn left(&self) -> Option<ExpressionNode>;
    fn right(&self) -> Option<ExpressionNode>;
}

pub trait UnaryExpression {
    fn operand(&self) -> Option<ExpressionNode>;
}

pub trait VariadicExpr {
    fn list(&self) -> Option<ExpressionListNode>;
}

macro_rules! impl_bin_expr {
    ($name:ident) => {
        impl BinaryExpression for $name {
            fn left(&self) -> Option<ExpressionNode> {
                $name::left(self)
            }

            fn right(&self) -> Option<ExpressionNode> {
                $name::right(self)
            }
        }
    };
}

impl_bin_expr!(WhenExpressionNode);
impl_bin_expr!(FbyExpressionNode);
impl_bin_expr!(ArrowExpressionNode);
impl_bin_expr!(AndExpressionNode);
impl_bin_expr!(OrExpressionNode);
impl_bin_expr!(XorExpressionNode);
impl_bin_expr!(ImplExpressionNode);
impl_bin_expr!(EqExpressionNode);
impl_bin_expr!(NeqExpressionNode);
impl_bin_expr!(LtExpressionNode);
impl_bin_expr!(LteExpressionNode);
impl_bin_expr!(GtExpressionNode);
impl_bin_expr!(GteExpressionNode);
impl_bin_expr!(DivExpressionNode);
impl_bin_expr!(ModExpressionNode);
impl_bin_expr!(SubExpressionNode);
impl_bin_expr!(AddExpressionNode);
impl_bin_expr!(MulExpressionNode);
impl_bin_expr!(PowerExpressionNode);
impl_bin_expr!(HatExpressionNode);

macro_rules! impl_un_expr {
    ($name:ident) => {
        impl UnaryExpression for $name {
            fn operand(&self) -> Option<ExpressionNode> {
                $name::operand(self)
            }
        }
    };
}

impl_un_expr!(NotExpressionNode);
impl_un_expr!(NegExpressionNode);
impl_un_expr!(PreExpressionNode);
impl_un_expr!(CurrentExpressionNode);
impl_un_expr!(IntExpressionNode);
impl_un_expr!(RealExpressionNode);

macro_rules! impl_variadic_expr {
    ($name:ident) => {
        impl VariadicExpr for $name {
            fn list(&self) -> Option<ExpressionListNode> {
                $name::list(self)
            }
        }
    };
}

impl_variadic_expr!(DieseExpressionNode);
impl_variadic_expr!(NorExpressionNode);
