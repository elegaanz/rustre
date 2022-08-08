use super::Error as RustreParseError;
use crate::rowan_nom::*;

use crate::lexer::Token::{self, *};

type Lang = crate::LustreLang;
type Children = crate::rowan_nom::Children<Lang, super::Error>;
type Input<'slice, 'src> = crate::rowan_nom::Input<'slice, 'src, Lang>;
type IResult<'slice, 'src, E = super::Error> =
    crate::rowan_nom::IResult<'slice, 'src, Lang, super::Error, E>;
type RootIResult<'slice, 'src, E = super::Error> =
    crate::rowan_nom::RootIResult<'slice, 'src, Lang, super::Error, E>;

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
    // FIXME: not all possibilities are handled (see rustdoc comment just above)
    alt((
        parse_include,
        constant_decl::parse_const_decl,
        type_decl::parse_type_decl,
        ext_nodes::parse_ext_node_decl,
        nodes::parse_node_decl,
    ))(input)
}

// Ebnf group PackageRules

// Ebnf group ModelRules

// Ebnf group IdentRules

pub fn parse_lv6_id_ref<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        t(Ident),
        opt(join((
            t_raw(DoubleColon),
            expect(t_raw(Ident), "expected ident after colon"),
        ))),
    ))(input)
}

pub fn parse_lv6_id<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((parse_lv6_id_ref, opt(parse_pragma)))(input)
}

pub fn parse_pragma<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PragmaNode,
        join((t(Percent), t(Ident), t(Colon), t(Ident), t(Percent))),
    )(input)
}

/// Parses an Lv6Id or an Lv6IdRef wrapped in an [`IdNode`], with an optional pragma
///
/// This parser should be used when parsing any ID, even when only one of the two types should
/// actually be used. This makes the parser more lax and allows for better diagnostics.
///
/// # Tolerated syntax errors
///
///   * Pragma after Lv6IdRef, when it should only occur after an Lv6Id
///   * This parser is supposed to be called every time an Lv6Id or Lv6IdRef is expected, even if
///     only one of those is actually valid
#[rustfmt::skip]
fn parse_id_any<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        IdNode,
        join((
            t(Ident),
            opt(join((
                t_raw(DoubleColon),
                expect(t_raw(Ident), "expected ident after colon"),
            ))),
            opt(parse_pragma),
        )),
    )(input)
}

// Ebnf group NodesRules

// TODO move everything in there
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
            alt((t(Int), t(Bool), t(Real), parse_id_any)),
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

pub fn parse_body<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        BodyNode,
        many_delimited(t(Let), parse_equation, success, t(Tel)),
    )(input)
}

pub fn parse_equation_list<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many0(parse_equation)(input)
}

pub fn parse_equation<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        alt((parse_equation_assert, parse_equation_equals)),
        expect(t(Semicolon), "expected semicolon after equation"),
    ))(input)
}

fn parse_equation_assert<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        AssertEquationNode,
        join((
            t(Assert),
            expect(
                expression::parse_expression,
                "expected expression after `assert`",
            ),
        )),
    )(input)
}

fn parse_equation_equals<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        EqualsEquationNode,
        join((
            alt((
                join((parse_left, expect(t(Equal), "missing `=` in equation"))),
                join((
                    expect(parse_left, "missing left operand in equation"),
                    t(Equal),
                )),
            )),
            expect(
                expression::parse_expression,
                "expected at the end of equation",
            ),
        )),
    )(input)
}

// Ebnf group LeftRules

pub fn parse_left<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        LeftNode,
        alt((
            many_delimited(success, parse_left_item, t(Comma), peek(t(Equal))),
            many_delimited(t(OpenPar), parse_left_item, t(Comma), t(ClosePar)),
        )),
    )(input)
}

pub fn parse_left_item<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fold_many1(
        parse_id_any,
        alt((
            map(join((t(Dot), parse_id_any)), |c| (c, LeftFieldAccessNode)),
            map(
                join((
                    t(OpenBracket),
                    expect(
                        alt((expression::parse_expression, parse_select)),
                        "expected expression or select",
                    ),
                    t(CloseBracket),
                )),
                |c| (c, LeftTableAccessNode),
            ),
        )),
        |a, (b, n)| (a + b).into_node(n),
    )(input)
}

pub fn parse_select<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        SelectNode,
        alt((
            expression::parse_expression_15,
            t(CDots),
            expect(
                expression::parse_expression_15,
                "expected second operand in step expression",
            ),
            parse_step,
        )),
    )(input)
}

pub fn parse_step<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    opt(node(
        StepNode,
        join((
            t(Step),
            expect(
                expression::parse_expression_16,
                "expected expression after `step`",
            ),
        )),
    ))(input)
}

// Ebnf group ExpressionRules

pub mod expression;

// Ebnf group MergeRules

// Ebnf group PredefRules

pub fn parse_predef_op<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PredefOp,
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
        )),
    )(input)
}

// Ebnf group ExpressionByNamesRules

// Ebnf group ConstantRules

pub fn parse_constant<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(ConstantNode, alt((t(True), t(False), t(IConst), t(RConst))))(input)
}
