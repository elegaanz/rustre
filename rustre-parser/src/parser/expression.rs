//! Expression-related parsers
//!
//! <https://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lv6-ref-man.pdf#section.2.10>

use super::*;

// TODO: is this function still needed?
pub fn expr_node<'slice, 'src: 'slice>(
    node: Token,
    parser: impl nom::Parser<Input<'slice, 'src>, Children, RustreParseError>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    super::node(node, parser)
}

macro_rules! parse_ops {
    ($operator:ident => $operator_node:ident) => {
        ::nom::Parser::map(t(Token::$operator), |c| (c, Token::$operator_node))
    };
    {$($operator:ident => $operator_node:ident,)*} => {
        alt((
            $(parse_ops!($operator => $operator_node),
            )*
        ))
    };
}

fn parse_expression_unary<'slice, 'src: 'slice>(
    operator: Token,
    operator_node: Token,
    mut next: impl nom::Parser<Input<'slice, 'src>, Children, RustreParseError>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    move |input| expr_node(operator_node, join((t(operator), |i| next.parse(i))))(input)
}

/// Parses a binary left-associative chain of expression
fn parse_expression_left<'slice, 'src: 'slice>(
    mut parse_operator: impl nom::Parser<Input<'slice, 'src>, (Children, Token), RustreParseError>,
    mut next: impl nom::Parser<Input<'slice, 'src>, Children, RustreParseError> + Copy,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    move |input| {
        fold_many1(
            next,
            |input| {
                let (input, (a, n)) = parse_operator.parse(input)?;
                let (input, b) = next.parse(input)?;
                Ok((input, (a + b, n)))
            },
            |a, (b, n)| (a + b).into_node(n),
        )(input)
    }
}

/// Parses a binary right-associative chain of expression
fn parse_expression_right<'slice, 'src: 'slice>(
    mut parse_operator: impl nom::Parser<Input<'slice, 'src>, (Children, Token), RustreParseError>,
    next: impl nom::Parser<Input<'slice, 'src>, Children, RustreParseError> + Copy,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    move |input| {
        fold_many1_right_expr(
            next,
            |input| parse_operator.parse(input),
            |a, (b, n)| (a + b).into_node(n),
        )(input)
    }
}

/// Parses a binary non-associative expression, or just one operand
///
/// # Tolerated syntax errors
///
///   * There may actually be more than two operands, but they should be treated as errors as
///     non-associativity in lustre disallows their chaining
fn parse_expression_no_assoc<'slice, 'src: 'slice>(
    mut parse_operator: impl nom::Parser<Input<'slice, 'src>, (Children, Token), RustreParseError>,
    mut next: impl nom::Parser<Input<'slice, 'src>, Children, RustreParseError>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    move |input| {
        let (mut input, mut left_expr) = next.parse(input)?;

        if let Ok((new_input, (operator, operator_node))) = parse_operator.parse(input.clone()) {
            input = new_input;
            left_expr += operator;

            let (new_input, operand) = next.parse(input)?;
            input = new_input;
            left_expr += operand;

            left_expr = left_expr.into_node(operator_node);
        }

        // Tolerance: Handle additional occurrences of the operator
        // FIXME: we should emit an error too
        while let Ok((new_input, (mut operator, _))) = parse_operator.parse(input.clone()) {
            input = new_input;

            let (new_input, operand) = next.parse(input)?;
            input = new_input;
            operator += operand;

            left_expr += operator.into_node(Error);
        }

        Ok((input, left_expr))
    }
}

pub fn parse_expression_terminal<'slice, 'src>(
    input: Input<'slice, 'src>,
) -> IResult<'slice, 'src> {
    alt((
        parse_constant,
        expression_by_names::parse_call_by_name_expression,
        expr_node(IdentExpressionNode, ident::parse_id_any),
        expr_node(
            MergeExpressionNode,
            join((
                t(Merge),
                expect(ident::parse_id_any, "expected identifier"),
                merge::parse_merge_cases,
            )),
        ),
        // TODO handle `with` separately
        expr_node(
            IfExpressionNode,
            // TODO check associativity compliance of `if then else`
            // TODO laxism
            join((
                alt((t(If), t(With))),
                parse_expression,
                t(Then),
                parse_expression,
                t(Else),
                parse_expression,
            )),
        ),
    ))(input)
}

// This one doesn't really exist but it is never mentioned in the spec
pub fn parse_expression_0<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fold_many1(
        parse_expression_terminal,
        alt((
            map(parse_call_par, |c| (c, CallByPosExpressionNode)),
            map(parse_array_brackets, |c| (c, ArrayAccessExpressionNode)),
        )),
        |a, (b, n)| (a + b).into_node(n),
    )(input)
}

pub fn parse_expression_1<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_right(parse_ops!(FBy => FbyExpressionNode), parse_expression_0)(input)
}

pub use parse_expression_1 as parse_expression_2; // TODO

pub fn parse_expression_3<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        expr_node(ParExpressionNode, parse_expression_list_par),
        expr_node(ArrayLiteralExpressionNode, parse_expression_list_bracket),
        parse_expression_2,
    ))(input)
}

pub fn parse_expression_4<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Hat => HatExpressionNode,
            Dot => FieldAccessExpressionNode,
        },
        parse_expression_3,
    )(input)
}

pub fn parse_expression_5<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        parse_expression_unary(Minus, NegExpressionNode, parse_expression_5),
        parse_expression_unary(Pre, PreExpressionNode, parse_expression_5),
        parse_expression_unary(Current, CurrentExpressionNode, parse_expression_5),
        parse_expression_unary(Current, CurrentExpressionNode, parse_expression_5),
        expr_node(
            DieseExpressionNode,
            join((
                t(Diese),
                expect(
                    many_delimited(t(OpenPar), parse_expression, t(Comma), t(ClosePar)),
                    "expected parenthesis-delimited expression after `#`",
                ),
            )),
        ),
        expr_node(
            NorExpressionNode,
            join((
                t(Nor),
                expect(
                    many_delimited(t(OpenPar), parse_expression, t(Comma), t(ClosePar)),
                    "expected parenthesis-delimited expression after `nor`",
                ),
            )),
        ),
        parse_expression_4,
    ))(input)
}

pub fn parse_expression_6<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        parse_expression_unary(Int, IntExpressionNode, parse_expression_6),
        parse_expression_unary(Real, RealExpressionNode, parse_expression_6),
        parse_expression_5,
    ))(input)
}

pub fn parse_expression_7<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fold_many1(
        parse_expression_6,
        join((
            t(When),
            expect(parse_clock_expr, "expected clock expression after `when`"),
        )),
        |a, b| (a + b).into_node(WhenExpressionNode),
    )(input)
}

pub fn parse_expression_8<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(parse_ops!(Power => PowerExpressionNode), parse_expression_7)(input)
}

pub fn parse_expression_9<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Star => MulExpressionNode,
            Slash => DivExpressionNode,
            Div => DivExpressionNode,
            Percent => ModExpressionNode,
            Mod => ModExpressionNode,
        },
        parse_expression_8,
    )(input)
}

pub fn parse_expression_10<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Plus => AddExpressionNode,
            Minus => SubExpressionNode,
        },
        parse_expression_9,
    )(input)
}

pub fn parse_expression_11<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        parse_expression_unary(Not, NotExpressionNode, parse_expression_11),
        parse_expression_10,
    ))(input)
}

pub fn parse_expression_12<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_no_assoc(
        parse_ops! {
            Lt => LtExpressionNode,
            Lte => LtExpressionNode,
            Equal => EqExpressionNode,
            Gt => GtExpressionNode,
            Gte => GtExpressionNode,
            Neq => NeqExpressionNode,
        },
        parse_expression_11,
    )(input)
}

pub fn parse_expression_13<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(parse_ops!(And => AndExpressionNode), parse_expression_12)(input)
}

pub fn parse_expression_14<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Or => OrExpressionNode,
            Xor => XorExpressionNode,
        },
        parse_expression_13,
    )(input)
}

pub fn parse_expression_15<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_right(parse_ops!(Impl => ImplExpressionNode), parse_expression_14)(input)
}

pub use parse_expression_15 as parse_expression_16;
pub use parse_expression_16 as parse_expression_17;

pub fn parse_expression_18<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops!(Arrow => ArrowExpressionNode),
        parse_expression_17,
    )(input)
}

pub fn parse_expression_19<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(parse_ops!(Bar => ConcatExpressionNode), parse_expression_18)(input)
}

pub fn parse_expression_20<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_19(input)
}

pub fn parse_expression<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_20(input)
}

pub fn parse_expression_list<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many_delimited(success, parse_expression, t(Comma), success)(input)
}

pub fn parse_expression_list_par<'slice, 'src>(
    input: Input<'slice, 'src>,
) -> IResult<'slice, 'src> {
    many_delimited(t(OpenPar), parse_expression, t(Comma), t(ClosePar))(input)
}

pub fn parse_expression_list_bracket<'slice, 'src>(
    input: Input<'slice, 'src>,
) -> IResult<'slice, 'src> {
    many_delimited(t(OpenBracket), parse_expression, t(Comma), t(CloseBracket))(input)
}

fn parse_call_par<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        opt(static_rules::parse_static_args),
        parse_expression_list_par,
    ))(input)
}

fn parse_array_brackets<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        t(OpenBracket),
        expect(
            alt((left::parse_select, parse_expression)),
            "expected expression",
        ),
        expect(t(CloseBracket), "expected closing square bracket"),
    ))(input)
}

pub fn parse_clock_expr<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ClockExpressionNode,
        alt((
            join((
                ident::parse_id_any,
                opt(join((
                    t(OpenPar),
                    expect(ident::parse_id_any, "expected identifier"),
                    t(ClosePar),
                ))),
            )),
            join((
                t(Not),
                alt((
                    ident::parse_id_any,
                    join((t(OpenPar), ident::parse_id_any, t(ClosePar))),
                )),
            )),
        )),
    )(input)
}
