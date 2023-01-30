use super::*;

pub fn parse_typed_lv6_ids<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypedIdsNode,
        join((
            ident::parse_lv6_id,
            many0(join((
                t(Comma),
                expect(ident::parse_lv6_id, "expected another identifier after `,`"),
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
            ident::parse_lv6_id,
            many0(join((
                t(Comma),
                expect(ident::parse_lv6_id, "expected other field name after `,`"),
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

/// Parses a (potentially `unsafe`) `node` or `function` declaration
///
/// # Tolerated syntax errors
///
///   * Omitting the `〈Params〉 returns 〈Params〉` part is only allowed if defining a node alias (when
///     an `=` sign follows)
pub fn parse_node_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        NodeNode,
        join((
            parse_node_type,
            expect(ident::parse_id_any, "missing function or node name"),
            static_rules::parse_static_params,
            opt(parse_params_and_returns),
            expect(
                alt((parse_node_decl_alias, parse_node_decl_definition)),
                "missing node definition, expected body or alias",
            ),
        )),
    )(input)
}

/// Parses `node`, `function`, `unsafe node` and `unsafe function`
///
/// This function accepts anything that matches at least one token ; the keyword "`unsafe`" will be
/// matched even if it isn't followed by `node` or `function`
pub fn parse_node_type<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        t(Node),
        t(Function),
        join((
            t(Unsafe),
            expect(
                alt((t(Node), t(Function))),
                "expected `node` or `function` after `unsafe`",
            ),
        )),
    ))(input)
}

/// Loosely parses `〈Params〉 returns 〈Params〉`
///
/// Either the first `〈Params〉` or the `returns` token must be present for the parser not to fail.
pub fn parse_params_and_returns<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        NodeProfileNode,
        alt((
            join((
                parse_params,
                expect(t(Returns), "expected `returns` after params"),
                expect(parse_params, "expected `(params...)` after `returns`"),
            )),
            // if the user forgot the first params, we can still attempt to parse using the `returns`
            // token
            join((
                // FIXME: define a parser that immediately fails instead of attempting to parse
                //        something we already know is missing
                expect(parse_params, "missing params before `returns`"),
                t(Returns),
                expect(parse_params, "expected `(params...)` after `returns`"),
            )),
        )),
    )(input)
}

/// Parses the end of a `NodeDecl`, where a definition is expected
/// (` [ ; ] 〈LocalDecls〉 〈Body〉 ( . | [ ; ] )`)
///
/// # See also
///
///   * [`parse_node_decl_alias`].
///
/// Both are called by [`parse_node_decl`]
fn parse_node_decl_definition<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        opt(t(Semicolon)),
        opt(parse_local_decl_list),
        body::parse_body,
        opt(alt((t(Dot), t(Semicolon)))),
    ))(input)
}

/// Parses the end of a `NodeDecl`, where an alias is expected
/// (` = 〈EffectiveNode〉 [ ; ]`)
///
/// # See also
///
///   * [`parse_node_decl_definition`].
///
/// Both are called by [`parse_node_decl`]
fn parse_node_decl_alias<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    join((
        // TODO: unexpect(Semicolon), (eat semicolon but consider it a syntax error)
        t(Equal),
        expect(
            static_rules::parse_effective_node,
            "missing aliased node name",
        ),
        opt(t(Semicolon)),
    ))(input)
}

pub fn parse_params<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ParamsNode,
        many_delimited(
            t(OpenPar),
            parse_var_decl,
            t(Semicolon),
            join((opt(t(Semicolon)), t(ClosePar))),
        ),
    )(input)
}

pub fn parse_local_decl_list<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many0(parse_one_local_decl)(input)
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
    // TODO: find out why it is faster to do like this instead of directly expect(many_del...())
    join((
        t(Var),
        expect(
            many_delimited(
                success,
                join((
                    expect(parse_var_decl, "malformed variable declaration"),
                    t(Semicolon),
                )),
                success,
                peek(alt((t(Const), t(Var), t(Let)))),
            ),
            "missing variable name",
        ),
    ))(input)
}

pub fn parse_var_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(VarDeclNode, alt((
        join((parse_typed_lv6_ids, opt(join((
            t(When),
            expect(expression::parse_clock_expr, "expected clock expression"),
        ))))),
        join((
            many_delimited(t(OpenPar), parse_typed_lv6_ids, t(Semicolon), t(ClosePar)),
            expect(t(When), "expected `when` to complete clock expression, remove previous parenthesis if you didn't mean to start a clock expression"),
            expect(expression::parse_clock_expr, "expected clock expression"),
        )),
    )))(input)
}
