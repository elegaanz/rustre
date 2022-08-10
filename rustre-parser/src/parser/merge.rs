use super::*;

pub fn parse_merge_cases<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many0(parse_merge_case)(input)
}

pub fn parse_merge_case<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        MergeCaseNode,
        join((
            t(OpenPar),
            alt((t(True), t(False), ident::parse_id_any)),
            t(Arrow),
            expression::parse_expression,
            t(ClosePar),
        )),
    )(input)
}
