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
pub fn parse_id_any<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
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
