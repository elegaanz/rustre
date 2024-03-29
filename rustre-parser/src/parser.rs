//! Individual parsers for Lustre's grammar
//!
//! If you want to parse an entire program, you'll mostly use [`parse_program`]. However, this
//! module gives you access to finer parsers, in case you need to build the syntax tree of a very
//! specific element. Most parsers in this file are grouped in submodules [according to the same
//! "Ebnf groups" that appear in the official specification][spec]. There is however no guarantee
//! that all parsers here map 1:1 to a parsing rule from the specification.
//!
//! # Errors
//!
//! While parsing an entire program isn't supposed to make the actual parser fail, if you go about
//! parsing individual syntax elements using a parser from this module, their parsers may fail,
//! typically if the first (few) token(s) are/is unexpected.
//!
//! [spec]: https://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lv6-ref-man.pdf

use super::ParserError as RustreParseError;
use rowan_nom::*;

use crate::lexer::Token::{self, *};

type Lang = crate::LustreLang;
type Children = rowan_nom::Children<Lang, super::ParserError>;
type Input<'slice, 'src> = rowan_nom::Input<'slice, 'src, Lang>;
type IResult<'slice, 'src, E = super::ParserError> =
    rowan_nom::IResult<'slice, 'src, Lang, super::ParserError, E>;
type RootIResult<'slice, 'src, E = super::ParserError> =
    rowan_nom::RootIResult<'slice, 'src, Lang, super::ParserError, E>;

// Utils

pub fn many_delimited<'slice, 'src: 'slice, IE: RowanNomError<Lang>>(
    mut left: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut repeat: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut separator: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src, IE> {
    use std::ops::ControlFlow;

    fn preceded_with_junk<'slice, 'src: 'slice, IE: RowanNomError<Lang>>(
        mut parser: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
        mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    ) -> impl FnMut(
        Input<'slice, 'src>,
    ) -> nom::IResult<Input<'slice, 'src>, ControlFlow<Children, Children>, IE> {
        move |mut input| {
            let mut children = Children::empty();

            loop {
                if let Ok((input, new_children)) = right.parse(input.clone()) {
                    break Ok((input, ControlFlow::Break(children + new_children)));
                } else if let Ok((input, new_children)) = parser.parse(input.clone()) {
                    break Ok((input, ControlFlow::Continue(children + new_children)));
                } else if let Ok((new_input, new_children)) =
                    t_any::<_, _, DummyError>(input.clone())
                {
                    input = new_input;
                    // TODO: more specific "UnexpectedToken" node below ?
                    children += new_children.into_node(Error);

                    let err = super::ParserError::from_message("many_preceded is skipping");
                    children += Children::from_err(err);
                } else {
                    // TODO: maybe don't error, but consider eof as a RIGHT equivalent + silent error
                    break Err(nom::Err::Error(IE::from_unexpected_eof(input.src_pos())));
                }
            }
        }
    }

    macro_rules! preceded_with_junk {
        ($parser:expr, $input:expr, &mut $children:ident) => {
            match preceded_with_junk(|i| $parser.parse(i), |i| right.parse(i))($input)? {
                (input, ControlFlow::Break(new_children)) => {
                    return Ok((input, $children + new_children));
                }
                (input, ControlFlow::Continue(new_children)) => {
                    $children += new_children;
                    input
                }
            }
        };
    }

    move |input| {
        let (input, mut children) = left.parse(input)?;

        let mut input = preceded_with_junk!(repeat, input, &mut children);

        loop {
            input = preceded_with_junk!(separator, input, &mut children);
            input = preceded_with_junk!(repeat, input, &mut children);
        }
    }
}

// Ebnf group ProgramRules

/// Parses en entire Lustre file (entry point to the rustre nom parser)
///
/// # Tolerated syntax errors
///
///   * Top level declarations may be in the wrong order (i.e. `include` at the end of a file), and
///     some mutually exclusive top-level declarations may be present at the same time
pub fn parse_program<'slice, 'src>(input: Input<'slice, 'src>) -> RootIResult<'slice, 'src> {
    root_node(
        Root,
        many_delimited(success, parse_top_level_decl, success, eof),
    )(input)
}

pub fn parse_include<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(IncludeStatement, join((t(Include), fallible(t(Str)))))(input)
}

/// Include or OneDecl (ConstDecl, TypeDecl, ExtNodeDecl, NodeDecl) or OnePack
pub fn parse_top_level_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        parse_include,
        constant_decl::parse_const_decl,
        type_decl::parse_type_decl,
        ext_nodes::parse_ext_node_decl,
        nodes::parse_node_decl,
        model::parse_model_decl,
        package::parse_pack_decl,
        package::parse_pack_eq,
    ))(input)
}

// Ebnf group PackageRules

pub mod package;

// Ebnf group ModelRules

pub mod model;

// Ebnf group IdentRules

pub mod ident;

// Ebnf group NodesRules

pub mod nodes;

// Ebnf group ConstantDeclRules

pub mod constant_decl;

// Ebnf group TypeDeclRules

pub mod type_decl;

// Ebnf group SimpleTypeRules

pub fn parse_type<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypeNode,
        join((
            alt((t(Int), t(Bool), t(Real), ident::parse_id_any)),
            parse_type_hat,
        )),
    )(input)
}

fn parse_type_hat<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    opt(join((
        t(Hat),
        expect(expression::parse_expression, "expected expression"),
    )))(input)
}

// Ebnf group ExtNodesRules

pub mod ext_nodes;

// Ebnf group StaticRules

pub mod static_rules;

// Ebnf group BodyRules

pub mod body;

// Ebnf group LeftRules

pub mod left;

// Ebnf group ExpressionRules

pub mod expression;

// Ebnf group MergeRules

pub mod merge;

// Ebnf group PredefRules

fn parse_predef_op_t<'slice, 'src: 'slice, P>(
    mut t: impl FnMut(Token) -> P,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src>
where
    P: nom::Parser<Input<'slice, 'src>, Children, super::ParserError>,
{
    move |input| {
        alt((
            t(Not),
            t(FBy),
            t(Pre),
            t(Current),
            t(Arrow),
            t(And),
            t(Or),
            t(Xor),
            t(Impl),
            t(Equal),
            t(Neq),
            t(Lt),
            t(Lte),
            t(Gt),
            t(Gte),
            t(Div),
            t(Mod),
            t(Minus),
            t(Plus),
            t(Slash),
            t(Star),
            t(If),
        ))(input)
    }
}

pub fn parse_predef_op<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(PredefOp, parse_predef_op_t(t))(input)
}

// Ebnf group ExpressionByNamesRules

pub mod expression_by_names;

// Ebnf group ConstantRules

pub fn parse_constant<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(ConstantNode, alt((t(True), t(False), t(IConst), t(RConst))))(input)
}
