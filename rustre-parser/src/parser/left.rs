use super::*;

pub fn parse_left<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        LeftNode,
        alt((
            many_delimited(t(OpenPar), parse_left_item, t(Comma), t(ClosePar)),
            many_delimited(success, parse_left_item, t(Comma), peek(t(Equal))),
        )),
    )(input)
}

pub fn parse_left_item<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fold_many1(
        ident::parse_id_any,
        alt((
            map(join((t(Dot), ident::parse_id_any)), |c| {
                (c, LeftFieldAccessNode)
            }),
            map(
                many_delimited(
                    t(OpenBracket),
                    expect(
                        alt((parse_select, expression::parse_expression)),
                        "expected expression or select",
                    ),
                    eof,
                    t(CloseBracket),
                ),
                |c| (c, LeftTableAccessNode),
            ),
        )),
        |a, (b, n)| (a + b).into_node(n),
    )(input)
}

pub fn parse_select<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        SelectNode,
        join((
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
