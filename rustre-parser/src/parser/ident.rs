use super::*;

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
    many_delimited(
        parse_lv6_id_ref,
        parse_pragma,
        peek(t(Percent)),
        peek_neg(t(Percent)),
    )(input)
}

pub fn parse_pragma<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PragmaNode,
        many_delimited(
            t(Percent),
            expect(
                join((t(Ident), t(Colon), t(Ident))),
                "expected ident:ident inside pragma",
            ),
            eof,
            t(Percent),
        ),
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
pub fn parse_id_any<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        IdNode,
        many_delimited(
            join((
                t(Ident),
                opt(join((
                    t_raw(DoubleColon),
                    expect(alt((t_raw(Ident), parse_predef_op_t(t_raw))), "expected ident after colon"),
                ))),
            )),
            parse_pragma,
            success,
            peek_neg(t(Percent)),
        )
    )(input)
}
