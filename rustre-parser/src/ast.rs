use crate::lexer::Token;
use crate::location::Spanned;

use crate::{SyntaxNode, SyntaxToken};

pub mod generated;
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

#[derive(Debug)]
/// A parsed source file.
///
/// This syntax tree is not typed.
///
/// ## Lifetime parameters
///
/// - `'a` is the lifetime of the source code
/// - `'f` is the lifetime of the file path
pub struct Ast<'a, 'f> {
    /// A list of files this file includes
    pub includes: Vec<Spanned<'f, &'a str>>,
    /// The declarations this file contains
    pub decls: Vec<Spanned<'f, Decl<'a, 'f>>>,
}

#[derive(Debug)]
/// A single declaration
pub enum Decl<'a, 'f> {
    /// Constant declaration
    Const {
        /// Name of the constant
        name: Spanned<'f, Ident<'a, 'f>>,
        /// The value of the constant.
        ///
        /// May be None for external constants
        value: Option<Spanned<'f, Expr<'a, 'f>>>,
        /// The type of the constant, if specified.
        ty: Option<Spanned<'f, TyExpr<'a, 'f>>>,
    },
    /// Type declaration
    Ty {
        /// Name of the type
        name: Spanned<'f, Ident<'a, 'f>>,
        /// Value of the type
        value: Spanned<'f, TyDecl<'a, 'f>>,
    },
    /// An external node.
    ExternalNode {
        /// Is it unsafe
        is_unsafe: bool,
        /// Is it a functional node
        is_function: bool,
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// The static parameters it takes
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        /// The dynamic parameters it takes
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// Its outputs
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
    },
    /// A local node
    Node {
        /// Is it unsafe
        is_unsafe: bool,
        /// Is it a functional node
        is_function: bool,
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// The static parameters it takes
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        /// The dynamic parameters it takes
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// Its outputs
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// The local variables of this node
        vars: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// The local constants of this node
        consts: Vec<Spanned<'f, Decl<'a, 'f>>>,
        /// The body of the node.
        ///
        /// It is a list of assertions and equations.
        body: Vec<Spanned<'f, BodyItem<'a, 'f>>>,
    },
    /// An alias node
    AliasNode {
        /// Is it unsafe
        is_unsafe: bool,
        /// Is it a functional node
        is_function: bool,
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// The static parameters it takes
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        /// The dynamic parameters it takes
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// Its outputs
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// The value of the aliasing
        effective_node: Spanned<'f, (Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>)>,
    },
    /// A package model
    Model {
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// What packages this model uses
        uses: Vec<Spanned<'f, Ident<'a, 'f>>>,
        /// What functions/nodes/types/constants this model needs
        needs: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        /// What functions/nodes/types/constants this model should provides
        provides: Vec<Spanned<'f, AbstractDecl<'a, 'f>>>,
        /// The body of the model
        body: Vec<Spanned<'f, Decl<'a, 'f>>>,
    },
    /// A package alias
    PackageAlias {
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// The model this package is based on
        model: Spanned<'f, Ident<'a, 'f>>,
        /// The parameters of the model
        static_args: Vec<(Spanned<'f, Ident<'a, 'f>>, StaticArg<'a, 'f>)>,
    },
    /// A package definition
    Package {
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// What packages it uses
        uses: Vec<Spanned<'f, Ident<'a, 'f>>>,
        /// What definitions it provides
        provides: Vec<Spanned<'f, AbstractDecl<'a, 'f>>>,
        /// Its body, which is a collection of declarations
        body: Vec<Spanned<'f, Decl<'a, 'f>>>,
    },
}

#[derive(Debug, Clone)]
/// Abstract declarations, used in models
pub enum AbstractDecl<'a, 'f> {
    /// Constant
    Const {
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// Its type
        ty: Spanned<'f, TyExpr<'a, 'f>>,
        /// A value, if defined
        def: Option<Spanned<'f, Expr<'a, 'f>>>,
    },
    /// Type
    Ty {
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// Its definition
        value: Spanned<'f, TyDecl<'a, 'f>>,
    },
    /// Node
    Node {
        /// Is it unsafe
        is_unsafe: bool,
        /// Is it a functional node
        is_function: bool,
        /// Its name
        name: Spanned<'f, Ident<'a, 'f>>,
        /// Its static parameters
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        /// Its dynamic parameters
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        /// Its outputs
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
    },
}

#[derive(Debug, Clone)]
/// Static argument
///
/// This is not a declaration, but the possible "expression" for static
/// parameters when calling a node.
///
/// The declaration of static parameters is represented by [`StaticParamDecl`].
pub enum StaticArg<'a, 'f> {
    /// A regular expression
    Expr(Spanned<'f, Expr<'a, 'f>>),
    /// A predefined operation
    Predefined(Spanned<'f, PredefinedItem>),
    /// A type
    Ty(Spanned<'f, TyExpr<'a, 'f>>),
    /// A node
    Node(Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>),
    /// Maybe a node name, maybe some constant, maybe something else, who knows?
    AmbiguousIdent(Ident<'a, 'f>),
}

#[derive(Debug, Clone)]
/// A predefined operation
pub enum PredefinedItem {
    /// Unary operator
    Unary(UnaryOp),
    /// Binary operator
    Binary(BinaryOp),
    /// N-ary operator
    NAry(NAryOp),
}

#[derive(Clone, Debug)]
/// Declaration of a static parameter
///
/// For the possible values that can be passed as a static argument
/// when calling a node, see [`StaticArg`].
pub enum StaticParamDecl<'a, 'f> {
    /// Constant
    Const {
        name: Spanned<'f, Ident<'a, 'f>>,
        ty: Spanned<'f, TyExpr<'a, 'f>>,
    },
    /// Type
    Ty { name: Spanned<'f, Ident<'a, 'f>> },
    /// Node
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, Ident<'a, 'f>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
    },
}

#[derive(Clone, Debug)]
/// Variable declaration
///
/// For variable declarations that may have a value, see [`ValuedVariableDecl`].
pub struct VariableDecl<'a, 'f> {
    /// Its name
    pub name: Spanned<'f, Ident<'a, 'f>>,
    /// Its type
    pub ty: Spanned<'f, TyExpr<'a, 'f>>,
    /// If None, the base clock is used
    pub clock: Option<Spanned<'f, ClockExpr<'a, 'f>>>,
}

#[derive(Clone, Debug)]
/// A variable declaration, which may have an initial value
pub struct ValuedVariableDecl<'a, 'f> {
    /// Its name
    pub name: Spanned<'f, Ident<'a, 'f>>,
    /// Its type
    pub ty: Spanned<'f, TyExpr<'a, 'f>>,
    /// If None, the base clock is used
    pub clock: Option<Spanned<'f, ClockExpr<'a, 'f>>>,
    /// Its initial value
    pub value: Option<Spanned<'f, Expr<'a, 'f>>>,
}

#[derive(Clone, Debug)]
/// Represents a clock
pub struct ClockExpr<'a, 'f>(
    /// I have no idea what these fields are
    ///
    /// TODO: find their meaning and document it
    pub Option<Spanned<'f, Ident<'a, 'f>>>,
    /// I have no idea what these fields are
    ///
    /// TODO: find their meaning and document it
    pub Box<Option<Spanned<'f, Ident<'a, 'f>>>>,
);

#[derive(Clone, Debug)]
/// A type expression
pub enum TyExpr<'a, 'f> {
    /// Base type: `int`
    Int,
    /// Base type: `bool`
    Bool,
    /// Base type: `real`
    Real,
    /// A named type.
    ///
    /// Example: `a`, `my_type`
    Named(Ident<'a, 'f>),
    /// An array type
    ///
    /// Example: `a^5`, `bool^2^2`
    Power(Spanned<'f, Box<TyExpr<'a, 'f>>>, Spanned<'f, Expr<'a, 'f>>),
}

#[derive(Clone, Debug)]
/// A type declaration
pub enum TyDecl<'a, 'f> {
    /// External type
    External,
    /// Type alias
    Alias(TyExpr<'a, 'f>),
    /// Enumerated type
    Enum(
        /// The variants of the enum
        Vec<Spanned<'f, Ident<'a, 'f>>>,
    ),
    /// A structured type
    Struct(
        /// The fields of the structure
        Vec<Spanned<'f, ValuedVariableDecl<'a, 'f>>>,
    ),
}

#[derive(Clone, Debug)]
/// A constant value
pub enum ConstValue {
    /// Integer constant
    Int(i64),
    /// Real constant
    Float(f64),
    /// Boolean constant
    Bool(bool),
}

#[derive(Clone, Debug)]
/// An expression
pub enum Expr<'a, 'f> {
    /// Constant value
    Const(ConstValue),
    /// Identifier
    Ident(Ident<'a, 'f>),
    /// Unary operation
    Unary(
        /// Operator
        Spanned<'f, UnaryOp>,
        /// Operand
        Spanned<'f, Box<Expr<'a, 'f>>>,
    ),
    /// Binary operation
    Binary(
        /// Operator
        Spanned<'f, BinaryOp>,
        /// LHS
        Spanned<'f, Box<Expr<'a, 'f>>>,
        /// RHS
        Spanned<'f, Box<Expr<'a, 'f>>>,
    ),
    /// N-ary operation
    NAry(
        /// Operator
        NAryOp,
        /// Operands
        Spanned<'f, Vec<Spanned<'f, Expr<'a, 'f>>>>,
    ),
    /// Slice access (`tab[x..y]`)
    Slice(
        /// Array
        Spanned<'f, Box<Expr<'a, 'f>>>,
        /// Start
        Spanned<'f, Box<Expr<'a, 'f>>>,
        /// End
        Spanned<'f, Box<Expr<'a, 'f>>>,
        /// Step
        Option<Spanned<'f, Box<Expr<'a, 'f>>>>,
    ),
    /// Ternary expression
    Ternary {
        /// Operator
        op: TernaryOp,
        /// Condition
        condition: Spanned<'f, Box<Expr<'a, 'f>>>,
        /// Value if the condition is true
        then: Spanned<'f, Box<Expr<'a, 'f>>>,
        /// Value if the condition is false
        otherwise: Spanned<'f, Box<Expr<'a, 'f>>>,
    },
    /// A clock expression
    NamedClock(Spanned<'f, ClockExpr<'a, 'f>>),
    /// Node call
    ///
    /// Not sure what the difference with [`Expr::CallByPos`] is…
    /// TODO
    CallByName(
        /// The name of the node
        Spanned<'f, Ident<'a, 'f>>,
        /// The static arguments
        Vec<Spanned<'f, StaticArg<'a, 'f>>>,
        /// The dynamic arguments
        Vec<Spanned<'f, Expr<'a, 'f>>>,
    ),
    /// Structure creation
    CreateStruct {
        /// Type name
        ty_name: Spanned<'f, Ident<'a, 'f>>,
        /// Fields (name and value)
        fields: Vec<(Spanned<'f, Ident<'a, 'f>>, Spanned<'f, Expr<'a, 'f>>)>,
        /// Base structure (fields that are not specified will be copied from this structure)
        base_value: Option<Spanned<'f, Box<Expr<'a, 'f>>>>,
    },
    /// Merge operation
    Merge(
        /// Not sure what that is
        /// TODO
        Spanned<'f, Ident<'a, 'f>>,
        /// The different cases of the merge
        Vec<Spanned<'f, MergeCase<'a, 'f>>>,
    ),
    /// Node call
    ///
    /// Not sure what the difference with [`Expr::CallByName`] is…
    /// TODO
    CallByPos(
        /// The node name and its static arguments
        ///
        /// TODO: shouldn't that be two different fields?
        Spanned<'f, (Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>)>,
        /// The dynamic arguments
        Vec<Spanned<'f, Expr<'a, 'f>>>,
    ),
}

#[derive(Clone, Debug)]
/// A merge case
pub struct MergeCase<'a, 'f> {
    /// The LHS
    pub lhs: Spanned<'f, MergeLHS<'a, 'f>>,
    /// The expression
    pub expr: Spanned<'f, Expr<'a, 'f>>,
}

#[derive(Clone, Debug)]
/// LHS of a merge case
pub enum MergeLHS<'a, 'f> {
    True,
    False,
    Ident(Spanned<'f, Ident<'a, 'f>>),
}

#[derive(Clone, Debug)]
/// Unary operators
pub enum UnaryOp {
    Not,
    Minus,
    Pre,
    Current,
    IntToReal,
    RealToInt,
}

#[derive(Clone, Debug)]
/// Binary operators
pub enum BinaryOp {
    When,
    FBy,
    Default,
    And,
    Or,
    Impl,
    Equal,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Mod,
    Minus,
    Plus,
    Slash,
    Power,
    Concat,
    Index,
    Range,
    Div,
    Prod,
    Step,
    // TODO: maybe this could be merged with Mod?
    // same for Div/Slash btw
    Percent,
    FieldAccess,
    Hat,
}

#[derive(Debug, Clone)]
/// Ternary operators
pub enum TernaryOp {
    IfThenElse,
    WithThenElse,
}

#[derive(Debug, Clone)]
/// N-ary operators
pub enum NAryOp {
    Xor,
    Nor,
    Array,
    Tuple,
}

#[derive(Debug)]
/// An item in the body of a node
pub enum BodyItem<'a, 'f> {
    /// An assertion
    Assert(Spanned<'f, Expr<'a, 'f>>),
    /// An equation
    Equation(
        Vec<Spanned<'f, LeftItem<'a, 'f>>>,
        Spanned<'f, Expr<'a, 'f>>,
    ),
}

#[derive(Debug)]
/// The left side of an equation
pub enum LeftItem<'a, 'f> {
    /// Identifier
    Ident(Spanned<'f, Ident<'a, 'f>>),
    /// Tuple, made of many smaller left items
    Tuple(Spanned<'f, Vec<LeftItem<'a, 'f>>>),
    /// A field access (`my_struct.my_field = …`)
    Field(
        /// The structure
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        /// The field name
        Spanned<'f, Ident<'a, 'f>>,
    ),
    /// Array index
    ArrayIndex(
        /// The table
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        /// The index
        Spanned<'f, Expr<'a, 'f>>,
    ),
    /// Array slice
    ArraySlice(
        /// The array
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        /// Start
        Spanned<'f, Expr<'a, 'f>>,
        /// End
        Spanned<'f, Expr<'a, 'f>>,
        /// Step
        Option<Spanned<'f, Expr<'a, 'f>>>,
    ),
}

#[derive(Clone)]
/// An identifier
pub enum Ident<'a, 'f> {
    /// A short identifier: just a single "word"
    ///
    /// It may have "pragmas" next to it, to give it special
    /// properties
    Short {
        id: Spanned<'f, &'a str>,
        pragmas: Vec<(&'a str, &'a str)>,
    },
    /// A long ID: a package name followed by another identifier
    Long(Spanned<'f, &'a str>, Box<Ident<'a, 'f>>),
}

impl<'a, 'f> std::fmt::Debug for Ident<'a, 'f> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Short { id, .. } => {
                write!(f, "{} \x1b[38;5;240m@ {:?}\x1b[0m", id.item, id.span)
            }
            Ident::Long(Spanned { item: id, .. }, next) => {
                write!(f, "{} :: {:?}", id, next)
            }
        }
    }
}
