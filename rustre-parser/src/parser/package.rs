use super::*;

pub fn parse_pack_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PackageDeclNode,
        join((
            t(Package),
            expect(ident::parse_id_any, "missing package name"),
            parse_uses,
            model::parse_provides,
            parse_pack_decl_body,
        )),
    )(input)
}

pub fn parse_pack_decl_body<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PackageDeclBody,
        expect(
            many_delimited(t(Body), parse_top_level_decl, success, t(End)),
            "missing package body (`body` ... `end`)",
        ),
    )(input)
}

pub fn parse_uses<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    opt(node(
        UsesNode,
        many_delimited(
            t(Uses),
            expect(ident::parse_id_any, "missing use name"),
            t(Comma),
            t(Semicolon),
        ),
    ))(input)
}

pub fn parse_eq_or_is<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((t(Equal), t(Is)))(input)
}

pub fn parse_pack_eq<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PackageAliasNode,
        join((
            t(Package),
            expect(ident::parse_id_any, "missing package alias name"),
            parse_eq_or_is,
            expect(ident::parse_id_any, "missing aliased package name"),
            expect(
                node(
                    NamedStaticArgsNode,
                    many_delimited(
                        t(OpenPar),
                        static_rules::parse_named_static_arg,
                        alt((t(Comma), t(Semicolon))),
                        t(ClosePar),
                    ),
                ),
                "expected `();` to complete package alias",
            ),
            expect(t(Semicolon), "expected `;` to complete package alias"),
        )),
    )(input)
}
