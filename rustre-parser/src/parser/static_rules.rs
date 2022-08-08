use super::*;

pub fn parse_static_params<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    opt(node(
        StaticParamsNode,
        many_delimited(
            t(OpenStaticPar),
            parse_static_param,
            t(Semicolon),
            t(CloseStaticPar),
        ),
    ))(input)
}

pub fn parse_static_param<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        StaticParamNode,
        alt((
            join((
                t(Type),
                expect(ident::parse_id_any, "expected id after `type`"),
            )),
            join((
                t(Const),
                expect(ident::parse_id_any, "expected id after `const`"),
                expect(
                    join((t(Colon), expect(parse_type, "expected type after colon"))),
                    "const static param must specify a type",
                ),
            )),
            join((
                nodes::parse_node_type,
                expect(
                    join((
                        ident::parse_id_any,
                        expect(
                            nodes::parse_params_and_returns,
                            "signature must include params and return values",
                        ),
                    )),
                    "unterminated node signature",
                ),
            )),
        )),
    )(input)
}

pub fn parse_effective_node<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        EffectiveNodeNode,
        join((ident::parse_id_any, opt(parse_static_args))),
    )(input)
}

pub fn parse_static_args<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        StaticArgsNode,
        many_delimited(
            t(OpenStaticPar),
            parse_static_arg,
            alt((t(Comma), t(Semicolon))),
            t(CloseStaticPar),
        ),
    )(input)
}

pub fn parse_static_arg<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        StaticArgNode,
        alt((
            join((t(Type), expect(parse_type, "expected type"))),
            join((
                t(Const),
                expect(expression::parse_expression, "expected expression"),
            )),
            join((
                alt((t(Node), t(Function))),
                expect(parse_effective_node, "expected node"),
            )),
            parse_predef_op,
            parse_surely_node,
            parse_surely_type,
            // TODO: check if that works, normally it's a SimpleExp but I guess we can verify the
            //       simpleness of the expression at post-parsing time
            expression::parse_expression,
        )),
    )(input)
}

pub fn parse_named_static_arg<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(NamedStaticArgNode, t(todo!()))(input)
}

pub fn parse_surely_node<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        EffectiveNodeNode,
        join((ident::parse_id_any, parse_static_args)),
    )(input)
}

pub fn parse_surely_type<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypeNode,
        join((alt((t(Bool), t(Int), t(Real))), parse_type_hat)),
    )(input)
}
