// Auto-generated file, do not edit manually.
// If you want to make changes, either add a new impl block
// outside of this file, or edit the build.rs file to generate
// code differently.

use crate::SyntaxNode;
use crate::SyntaxToken;
use crate::ast::AstNode;
use crate::ast::AstToken;
use crate::lexer::Token;


// Root

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Root {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for Root {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Root
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to Root")
    }
}

impl Root {
    pub fn all_include_statement(&self) -> impl Iterator<Item = IncludeStatement> {
        self.syntax().children().filter_map(IncludeStatement::cast)
    }
    pub fn package_body(&self) -> Option<PackageBody> {
        self.syntax().children().find_map(PackageBody::cast)
    }
    pub fn package_list(&self) -> Option<PackageList> {
        self.syntax().children().find_map(PackageList::cast)
    }
}

// IncludeStatement

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IncludeStatement {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for IncludeStatement {
    fn can_cast(kind: Token) -> bool {
        kind == Token::IncludeStatement
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to IncludeStatement")
    }
}

// PackageBody

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageBody {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageBody {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageBody
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to PackageBody")
    }
}

impl PackageBody {
    pub fn all_one_decl(&self) -> impl Iterator<Item = OneDecl> {
        self.syntax().children().filter_map(OneDecl::cast)
    }
}

// PackageList

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageList {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageList {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageList
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to PackageList")
    }
}

impl PackageList {
    pub fn all_model_decl(&self) -> impl Iterator<Item = ModelDecl> {
        self.syntax().children().filter_map(ModelDecl::cast)
    }
    pub fn all_package_decl(&self) -> impl Iterator<Item = PackageDecl> {
        self.syntax().children().filter_map(PackageDecl::cast)
    }
    pub fn all_package_alias(&self) -> impl Iterator<Item = PackageAlias> {
        self.syntax().children().filter_map(PackageAlias::cast)
    }
}

// ModelDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModelDecl {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for ModelDecl {
    fn can_cast(kind: Token) -> bool {
        kind == Token::ModelDecl
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to ModelDecl")
    }
}

// PackageDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDecl {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageDecl {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageDecl
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to PackageDecl")
    }
}

// PackageAlias

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageAlias {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageAlias {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageAlias
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to PackageAlias")
    }
}

// OneDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OneDecl {
    ConstantDecl(ConstantDecl),
    TypeDecl(TypeDecl),
    NodeDecl(NodeDecl),
}
impl AstNode for OneDecl {
    fn can_cast(kind: Token) -> bool {
        ConstantDecl::can_cast(kind) || TypeDecl::can_cast(kind) || NodeDecl::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        ConstantDecl::cast(syntax.clone()).map(Self::ConstantDecl)
            .or_else(|| TypeDecl::cast(syntax.clone()).map(Self::TypeDecl))
            .or_else(|| NodeDecl::cast(syntax.clone()).map(Self::NodeDecl))
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ConstantDecl(x) => x.syntax(),
            Self::TypeDecl(x) => x.syntax(),
            Self::NodeDecl(x) => x.syntax(),
        }
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to OneDecl")
    }
}

impl OneDecl {
    pub fn is_constant_decl(&self) -> bool {
        if let OneDecl::ConstantDecl(_) = *self { true } else { false }
    }
    pub fn unwrap_constant_decl(self) -> ConstantDecl {
        if let OneDecl::ConstantDecl(data) = self { data } else { panic!("Failed to unwrap OneDecl as ConstantDecl") }
    }
    pub fn is_type_decl(&self) -> bool {
        if let OneDecl::TypeDecl(_) = *self { true } else { false }
    }
    pub fn unwrap_type_decl(self) -> TypeDecl {
        if let OneDecl::TypeDecl(data) = self { data } else { panic!("Failed to unwrap OneDecl as TypeDecl") }
    }
    pub fn is_node_decl(&self) -> bool {
        if let OneDecl::NodeDecl(_) = *self { true } else { false }
    }
    pub fn unwrap_node_decl(self) -> NodeDecl {
        if let OneDecl::NodeDecl(data) = self { data } else { panic!("Failed to unwrap OneDecl as NodeDecl") }
    }
}

// ConstantDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantDecl {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for ConstantDecl {
    fn can_cast(kind: Token) -> bool {
        kind == Token::ConstantDecl
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to ConstantDecl")
    }
}

// TypeDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDecl {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for TypeDecl {
    fn can_cast(kind: Token) -> bool {
        kind == Token::TypeDecl
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to TypeDecl")
    }
}

// NodeDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeDecl {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for NodeDecl {
    fn can_cast(kind: Token) -> bool {
        kind == Token::NodeDecl
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to NodeDecl")
    }
}

impl NodeDecl {
    pub fn is_extern(&self) -> bool {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).any(|s| Extern::cast(s).is_some())
    }
    pub fn is_unsafe(&self) -> bool {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).any(|s| Unsafe::cast(s).is_some())
    }
    pub fn is_node(&self) -> bool {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).any(|s| Node::cast(s).is_some())
    }
    pub fn is_function(&self) -> bool {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).any(|s| Function::cast(s).is_some())
    }
    pub fn r#extern(&self) -> Option<Extern> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Extern::cast)
    }
    pub fn r#unsafe(&self) -> Option<Unsafe> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Unsafe::cast)
    }
    pub fn node(&self) -> Option<Node> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Node::cast)
    }
    pub fn function(&self) -> Option<Function> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Function::cast)
    }
}

// Include

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Include {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Include {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Include
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Include")
    }
}

// Str

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Str {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Str {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Str
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Str")
    }
}

// Model

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Model {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Model {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Model
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Model")
    }
}

// Package

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Package {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Package {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Package
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Package")
    }
}

// Extern

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Extern {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Extern {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Extern
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Extern")
    }
}

// Unsafe

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unsafe {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Unsafe {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Unsafe
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Unsafe")
    }
}

// Node

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Node {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Node
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Node")
    }
}

// Function

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Function {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Function
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Function")
    }
}

// Const

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Const {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Const
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Const")
    }
}

// Type

/// Token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub(crate) syntax: SyntaxToken,
}

impl AstToken for Type {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Type
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
    fn expect(syntax: SyntaxToken) -> Self {
        Self::cast(syntax).expect("Failed to cast to Type")
    }
}
