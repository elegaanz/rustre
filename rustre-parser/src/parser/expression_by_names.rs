use super::*;

pub fn parse_call_by_name_expression<'slice, 'src>(
    input: Input<'slice, 'src>,
) -> IResult<'slice, 'src> {
    expression::expr_node(
        CallByNameExpressionNode,
        join((
            ident::parse_id_any,
            many_delimited(
                join((
                    t(OpenBrace),
                    opt(join((
                        expect(ident::parse_id_any, "expected ident before `with`"),
                        t(With),
                    ))),
                )),
                parse_call_by_name_param,
                alt((t(Semicolon), t(Comma))),
                join((opt(t(Semicolon)), t(CloseBrace))),
            ),
        )),
    )(input)
}

pub fn parse_call_by_name_param<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        CallByNameParamNode,
        alt((
            join((
                ident::parse_id_any,
                expect(
                    join((t(Equal), expression::parse_expression)),
                    "expected `= <value>` after field name",
                ),
            )),
            join((
                expect(ident::parse_id_any, "expected field name"),
                t(Equal),
                expect(expression::parse_expression, "expected value"),
            )),
        )),
    )(input)
}
