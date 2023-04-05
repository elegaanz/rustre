use super::*;

pub fn parse_type_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypeDeclNode,
        join((t(Type), expect(parse_type_decls, "missing type name"))),
    )(input)
}

pub fn parse_type_decls<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many_delimited(
        success,
        join((
            parse_one_type_decl,
            expect(t(Semicolon), "expected `;` after type declaration"),
        )),
        success,
        peek_neg(t(Ident)),
    )(input)
}

pub fn parse_one_type_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        OneTypeDeclNode,
        join((
            ident::parse_lv6_id,
            opt(join((
                t(Equal),
                expect(
                    alt((parse_type, parse_enum_decl, parse_struct_decl)),
                    "expected a struct/enum declaration or an existing type expression",
                ),
            ))),
        )),
    )(input)
}

fn parse_enum_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        EnumDeclNode,
        join((
            t(Enum),
            expect(
                many_delimited(t(OpenBrace), ident::parse_lv6_id, t(Comma), t(CloseBrace)),
                "missing `{}` after `enum`",
            ),
        )),
    )(input)
}

fn parse_struct_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        StructDeclNode,
        join((
            opt(t(Struct)),
            many_delimited(
                t(OpenBrace),
                nodes::parse_typed_valued_lv6_id,
                t(Semicolon),
                join((opt(t(Semicolon)), t(CloseBrace))),
            ),
        )),
    )(input)
}
