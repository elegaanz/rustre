use super::*;

/// Parses `extern node`, `extern function`, `unsafe extern node` and `unsafe extern function`
///
/// This function accepts anything that matches at least one token ; the keyword "`unsafe`" will be
/// matched even if it isn't followed by `node` or `function`
fn parse_ext_node_type<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        opt(t(Unsafe)),
        t(Extern),
        expect(
            alt((t(Node), t(Function))),
            "expected `node` or `function` after `extern`",
        ),
    ))(input)
}

pub fn parse_ext_node_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ExternalNodeDeclNode,
        join((
            parse_ext_node_type,
            expect(parse_id_any, "missing external node name"),
            expect(
                parse_params_and_returns,
                "missing signature (params and returned values)",
            ),
            opt(t(Semicolon)),
        )),
    )(input)
}
