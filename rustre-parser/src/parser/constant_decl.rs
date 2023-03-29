use super::*;

pub fn parse_const_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ConstantDeclNode,
        join((t(Const), expect(parse_const_decls, "missing constant name"))),
    )(input)
}

pub fn parse_const_decls<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    many_delimited(success, parse_one_const_decl, success, peek_neg(t(Ident)))(input)
}

/// # Tolerated syntax errors
///
///   * It shouldn't be possible to specify an initializer on a constant with multiple identifiers,
///     but the parser accepts it nonetheless
pub fn parse_one_const_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        OneConstantDeclNode,
        join((
            ident::parse_id_any,
            many0(join((
                t(Comma),
                expect(
                    ident::parse_id_any,
                    "expected other constant name after `,`",
                ),
            ))),
            opt(join((
                t(Colon),
                expect(parse_type, "unspecified constant type"),
            ))),
            opt(join((
                t(Equal),
                expect(expression::parse_expression, "missing constant initializer"),
            ))),
            expect(t(Semicolon), "expected `;` after const declaration"),
        )),
    )(input)
}
