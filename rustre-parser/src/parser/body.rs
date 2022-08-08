use super::*;

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
                join((
                    left::parse_left,
                    expect(t(Equal), "missing `=` in equation"),
                )),
                join((
                    expect(left::parse_left, "missing left operand in equation"),
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
