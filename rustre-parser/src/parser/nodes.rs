use super::*;

pub fn parse_typed_lv6_ids<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypedLv6IdsNode,
        join((
            parse_lv6_id,
            many0(join((
                t(Comma),
                expect(parse_lv6_id, "expected another identifier after `,`"),
            ))),
            expect(
                join((t(Colon), expect(parse_type, "missing type expression"))),
                "type must be specified explicitly",
            ),
        )),
    )(input)
}

pub fn parse_typed_valued_lv6_id<'slice, 'src>(
    input: Input<'slice, 'src>,
) -> IResult<'slice, 'src> {
    node(
        TypedValuedLv6IdNode,
        join((
            parse_lv6_id,
            many0(join((
                t(Comma),
                expect(parse_lv6_id, "expected other field name after `,`"),
            ))),
            opt(join((
                t(Colon),
                expect(parse_type, "unspecified struct field type"),
            ))),
            opt(join((
                t(Equal),
                expect(expression::parse_expression, "missing field initializer"),
            ))),
        )),
    )(input)
}

pub fn parse_local_decl_list<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((parse_one_local_decl, many0(parse_one_local_decl)))(input)
}

pub fn parse_one_local_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((parse_local_consts, parse_local_vars))(input)
}

pub fn parse_local_consts<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        t(Const),
        expect(
            constant_decl::parse_const_decls,
            "missing constant declaration",
        ),
    ))(input)
}

pub fn parse_local_vars<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many_delimited(t(Var), parse_var_decl, t(Semicolon), t(Semicolon))(input)
}

pub fn parse_var_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        join((parse_typed_lv6_ids, opt(join((
            t(When),
            expect(expression::parse_clock_expr, "expected clock expression"),
        ))))),
        join((
            many_delimited(t(OpenPar), parse_typed_lv6_ids, t(Semicolon), t(ClosePar)),
            expect(t(When), "expected `when` to complete clock expression, remove previous parenthesis if you didn't mean to start a clock expression"),
            expect(expression::parse_clock_expr, "expected clock expression"),
        )),
    ))(input)
}
