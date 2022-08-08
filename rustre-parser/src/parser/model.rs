use super::*;

pub fn parse_provides<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    opt(many_delimited(
        t(Provides),
        join((
            parse_provide,
            expect(t(Semicolon), "provides must be separated by semicolons"),
        )),
        success,
        peek(t(Body)),
    ))(input)
}

pub fn parse_provide<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ProvidesNode,
        alt((
            join((
                t(Const),
                expect(
                    join((t(Colon), expect(parse_type, "missing type after `:`"))),
                    "type of constant must be specified explicitly",
                ),
                opt(join((
                    t(Equal),
                    expect(
                        expression::parse_expression,
                        "expected expression initializer after `=`",
                    ),
                ))),
            )),
            join((
                nodes::parse_node_type,
                expect(ident::parse_id_any, "missing node name"),
                static_rules::parse_static_params,
                expect(nodes::parse_params_and_returns, "missing node signature"),
            )),
            join((
                t(Type),
                expect(type_decl::parse_one_type_decl, "expected type declaration"),
            )),
        )),
    )(input)
}

pub fn parse_model_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ModelDeclNode,
        join((
            t(Model),
            expect(ident::parse_id_any, "missing model name"),
            package::parse_uses,
            expect(
                many_delimited(
                    t(Needs),
                    static_rules::parse_static_param,
                    t(Semicolon),
                    join((t(Semicolon), peek(alt((t(Provides), t(Body)))))),
                ),
                "expected `needs` clause",
            ),
            parse_provides,
            package::parse_pack_decl_body,
        )),
    )(input)
}
