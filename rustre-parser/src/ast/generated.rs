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

impl IncludeStatement {
    pub fn include(&self) -> Include {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Include::cast).unwrap()
    }
    pub fn str(&self) -> Str {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Str::cast).unwrap()
    }
}

// ModelDeclNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModelDeclNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for ModelDeclNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::ModelDeclNode
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
        Self::cast(syntax).expect("Failed to cast to ModelDeclNode")
    }
}

impl ModelDeclNode {
    pub fn model(&self) -> Model {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Model::cast).unwrap()
    }
}

// PackageDeclNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDeclNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageDeclNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageDeclNode
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
        Self::cast(syntax).expect("Failed to cast to PackageDeclNode")
    }
}

impl PackageDeclNode {
    pub fn package(&self) -> Package {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Package::cast).unwrap()
    }
}

// PackageAliasNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageAliasNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageAliasNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageAliasNode
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
        Self::cast(syntax).expect("Failed to cast to PackageAliasNode")
    }
}

impl PackageAliasNode {
    pub fn package(&self) -> Package {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Package::cast).unwrap()
    }
}

// PackageDeclBody

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageDeclBody {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for PackageDeclBody {
    fn can_cast(kind: Token) -> bool {
        kind == Token::PackageDeclBody
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
        Self::cast(syntax).expect("Failed to cast to PackageDeclBody")
    }
}

impl PackageDeclBody {
    pub fn all_one_decl(&self) -> impl Iterator<Item = OneDecl> {
        self.syntax().children().filter_map(OneDecl::cast)
    }
}

// OneDecl

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OneDecl {
    ConstantDeclNode(ConstantDeclNode),
    TypeDeclNode(TypeDeclNode),
    NodeNode(NodeNode),
}
impl AstNode for OneDecl {
    fn can_cast(kind: Token) -> bool {
        ConstantDeclNode::can_cast(kind) || TypeDeclNode::can_cast(kind) || NodeNode::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        ConstantDeclNode::cast(syntax.clone()).map(Self::ConstantDeclNode)
            .or_else(|| TypeDeclNode::cast(syntax.clone()).map(Self::TypeDeclNode))
            .or_else(|| NodeNode::cast(syntax.clone()).map(Self::NodeNode))
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ConstantDeclNode(x) => x.syntax(),
            Self::TypeDeclNode(x) => x.syntax(),
            Self::NodeNode(x) => x.syntax(),
        }
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to OneDecl")
    }
}

impl OneDecl {
    pub fn is_constant_decl_node(&self) -> bool {
        if let OneDecl::ConstantDeclNode(_) = *self { true } else { false }
    }
    pub fn unwrap_constant_decl_node(self) -> ConstantDeclNode {
        if let OneDecl::ConstantDeclNode(data) = self { data } else { panic!("Failed to unwrap OneDecl as ConstantDeclNode") }
    }
    pub fn is_type_decl_node(&self) -> bool {
        if let OneDecl::TypeDeclNode(_) = *self { true } else { false }
    }
    pub fn unwrap_type_decl_node(self) -> TypeDeclNode {
        if let OneDecl::TypeDeclNode(data) = self { data } else { panic!("Failed to unwrap OneDecl as TypeDeclNode") }
    }
    pub fn is_node_node(&self) -> bool {
        if let OneDecl::NodeNode(_) = *self { true } else { false }
    }
    pub fn unwrap_node_node(self) -> NodeNode {
        if let OneDecl::NodeNode(data) = self { data } else { panic!("Failed to unwrap OneDecl as NodeNode") }
    }
}

// ConstantDeclNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantDeclNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for ConstantDeclNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::ConstantDeclNode
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
        Self::cast(syntax).expect("Failed to cast to ConstantDeclNode")
    }
}

impl ConstantDeclNode {
    pub fn r#const(&self) -> Const {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Const::cast).unwrap()
    }
}

// TypeDeclNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDeclNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for TypeDeclNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::TypeDeclNode
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
        Self::cast(syntax).expect("Failed to cast to TypeDeclNode")
    }
}

impl TypeDeclNode {
    pub fn r#type(&self) -> Type {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(Type::cast).unwrap()
    }
}

// NodeNode

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NodeNode {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for NodeNode {
    fn can_cast(kind: Token) -> bool {
        kind == Token::NodeNode
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
        Self::cast(syntax).expect("Failed to cast to NodeNode")
    }
}

impl NodeNode {
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
