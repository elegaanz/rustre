use crate::rowan_nom::*;
use rowan::GreenNodeBuilder;

use crate::{
    lexer::Token::{self, *},
    Parse,
};

type Lang = crate::LustreLang;
type Children = crate::rowan_nom::Children<Lang, super::Error>;
type Input<'slice, 'src> = crate::rowan_nom::Input<'slice, 'src, Lang>;
type IResult<'slice, 'src, E = super::Error> =
    crate::rowan_nom::IResult<'slice, 'src, Lang, super::Error, E>;
type RootIResult<'slice, 'src, E = super::Error> =
    crate::rowan_nom::RootIResult<'slice, 'src, Lang, super::Error, E>;

// TODO remove when nom has replaced everything
pub struct Parser<'a> {
    /// Stack of remaining tokens
    ///
    /// The first token is at the end of the Vec (top of the stack)
    pub(crate) tokens: Vec<(Token, &'a str)>,
    pub(crate) builder: GreenNodeBuilder<'static>,
    pub(crate) errors: Vec<super::Error>,
    pub(crate) pos: usize,
}

/// Utils
#[allow(dead_code)]
impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<(Token, &'a str)>) -> Parse {
        let input = Input::from(tokens.as_slice());
        let (_, (root, errors)) = parse_program(input).expect("TODO");
        Parse { root, errors }
    }

    fn start(&mut self, tok: Token) {
        self.builder.start_node(tok.into())
    }

    fn end(&mut self) {
        self.builder.finish_node()
    }

    fn current(&mut self) -> Option<Token> {
        self.tokens.last().map(|x| x.0)
    }

    fn skip_trivia(&mut self) {
        while self
            .current()
            .map(crate::LustreLang::is_trivia)
            .unwrap_or(false)
        {
            self.next();
        }
    }

    fn next(&mut self) {
        if let Some((tok, source)) = self.tokens.pop() {
            self.pos += source.len();
            self.builder.token(tok.into(), source);
        }
    }

    /// Reports an error and skips one token
    fn error<S: ToString>(&mut self, msg: S) {
        self.start(Error);
        self.errors.push(super::Error {
            msg: msg.to_string(),
            span: self.start_pos()..self.end_pos(),
            cause: None,
        });
        self.next();
        self.end();
    }

    fn error_until(&mut self, msg: &str, stop: &[Token]) {
        self.start(Error);
        let start = self.start_pos();

        while let Some(curr) = self.current() {
            if stop.contains(&curr) {
                break;
            }
            self.next();
        }

        self.errors.push(super::Error {
            msg: msg.to_owned(),
            span: start..self.end_pos(),
            cause: None,
        });
        self.end();
    }

    fn peek<const N: usize>(&self) -> Option<[Token; N]> {
        let start = self.tokens.len() - 1 - N;
        self.tokens
            .iter()
            .map(|(t, _)| *t)
            .skip(start)
            .collect::<Vec<_>>()[..]
            .try_into()
            .ok()
    }

    /// Report an error if the current token is not the expected one,
    /// moves to the next token if it matched
    fn expect(&mut self, expected: Token) -> bool {
        self.skip_trivia();
        let current = self.current();
        if current != Some(expected) {
            self.error(format!(
                "Unexpected token: {:?} (expected {:?})",
                current, expected
            ));
            false
        } else {
            self.next();
            true
        }
    }

    /// Advance only if the next token is the given one
    ///
    /// Allows for optionally matching a token
    fn accept(&mut self, tok: Token) -> bool {
        self.skip_trivia();
        if self.current() == Some(tok) {
            self.next();
            true
        } else {
            false
        }
    }

    fn start_pos(&self) -> usize {
        self.pos
    }

    fn end_pos(&self) -> usize {
        self.pos + self.tokens.last().map(|x| x.1.len()).unwrap_or(0)
    }
}

static NEW_DECL: &[Token] = &[Const, Type, Node, Unsafe, Extern, Function, End];

// Utils

pub fn many_delimited<'slice, 'src: 'slice, IE: RowanNomError<Lang>>(
    mut left: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut repeat: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut separator: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src, IE> {
    use std::ops::ControlFlow;

    fn preceded_with_junk<'slice, 'src: 'slice, IE: RowanNomError<Lang>>(
        mut parser: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
        mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    ) -> impl FnMut(
        Input<'slice, 'src>,
    ) -> nom::IResult<Input<'slice, 'src>, ControlFlow<Children, Children>, IE> {
        move |mut input| loop {
            let mut children = Children::empty();

            if let Ok((input, new_children)) = right.parse(input.clone()) {
                break Ok((input, ControlFlow::Break(children + new_children)));
            } else if let Ok((input, new_children)) = parser.parse(input.clone()) {
                break Ok((input, ControlFlow::Continue(children + new_children)));
                // TODO: more specific "UnexpectedToken" node below ?
            } else if let Ok((new_input, new_children)) =
                node(Error, t_any::<_, _, DummyError>)(input.clone())
            {
                input = new_input;
                children += new_children;
            } else {
                // TODO: maybe don't error, but consider eof as a RIGHT equivalent + silent error
                break Err(nom::Err::Error(IE::from_unexpected_eof(input.src_pos())));
            }
        }
    }

    macro_rules! preceded_with_junk {
        ($parser:expr, $input:expr, &mut $children:ident) => {
            match preceded_with_junk(|i| $parser.parse(i), |i| right.parse(i))($input)? {
                (input, ControlFlow::Break(new_children)) => {
                    return Ok((input, $children + new_children));
                }
                (input, ControlFlow::Continue(new_children)) => {
                    $children += new_children;
                    input
                }
            }
        };
    }

    move |input| {
        let (input, mut children) = left.parse(input)?;

        let mut input = preceded_with_junk!(repeat, input, &mut children);

        loop {
            input = preceded_with_junk!(separator, input, &mut children);
            input = preceded_with_junk!(repeat, input, &mut children);
        }
    }
}

// Ebnf group ProgramRules

/// Parses en entire Lustre file (entry point to the rustre nom parser)
///
/// # Tolerated syntax errors
///
///   * Top level declarations may be in the wrong order (i.e. `include` at the end of a file), and
///     some mutually exclusive top-level declarations may be present at the same time
pub fn parse_program<'slice, 'src>(input: Input<'slice, 'src>) -> RootIResult<'slice, 'src> {
    root_node(
        Root,
        many_delimited(success, parse_top_level_decl, success, eof),
    )(input)
}

pub fn parse_include<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(IncludeStatement, join((t(Include), fallible(t(Str)))))(input)
}

/// Include or OneDecl (ConstDecl, TypeDecl, ExtNodeDecl, NodeDecl) or OnePack
pub fn parse_top_level_decl<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    // FIXME: not all possibilities are handled (see rustdoc comment just above)
    alt((parse_include, parse_node_decl))(input)
}

// Ebnf group PackageRules

// Ebnf group ModelRules

// Ebnf group IdentRules

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
fn parse_id_any<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
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

// Ebnf group NodesRules

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
            expect(parse_id_any, "missing function or node name"),
            parse_static_params,
            opt(parse_params_and_returns),
            opt(alt((parse_node_decl_definition, parse_node_decl_alias))),
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
fn parse_params_and_returns<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
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
    ))(input)
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
        // TODO local decls
        parse_body,
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
        // TODO: parse_effective_node,
        opt(t(Semicolon)),
    ))(input)
}

pub fn parse_params<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        ParamsNode,
        many_delimited(
            t(OpenPar),
            success, // TODO
            t(Comma),
            join((opt(t(Semicolon)), t(ClosePar))),
        ),
    )(input)
}

// Ebnf group ConstantDeclRules

// Ebnf group TypeDeclRules

// Ebnf group SimpleTypeRules

pub fn parse_type<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        TypeNode,
        join((
            alt((t(Int), t(Bool), t(Real), parse_id_any)),
            opt(join((
                t(Hat),
                expect(
                    adapter::old_style(Parser::expression),
                    "expected expression",
                ),
            ))),
        )),
    )(input)
}

// Ebnf group ExtNodesRules

// Ebnf group StaticRules

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
    node(todo!(), t(todo!()))(input)
}

pub fn parse_effective_node<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        EffectiveNodeNode,
        join((parse_id_any, opt(parse_static_args))),
    )(input)
}

pub fn parse_static_args<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(StaticArgsNode, t(todo!()))(input)
}

// Ebnf group BodyRules

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
                adapter::old_style(Parser::expression),
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
                join((parse_left, expect(t(Equal), "missing `=` in equation"))),
                join((
                    expect(parse_left, "missing left operand in equation"),
                    t(Equal),
                )),
            )),
            expect(
                adapter::old_style(Parser::expression),
                "expected at the end of equation",
            ),
        )),
    )(input)
}

// Ebnf group LeftRules

pub fn parse_left<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    // TODO: a Left is just slightly more complicated than this
    parse_id_any(input)
}

// Ebnf group ExpressionRules

// Ebnf group MergeRules

// Ebnf group PredefRules

pub fn parse_predef_op<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        PredefOp,
        alt((
            t(Not),
            t(FBy),
            t(Pre),
            t(Current),
            t(Arrow),
            t(And),
            t(Or),
            t(Xor),
            t(Impl),
            t(Equal),
            t(Neq),
            t(Lt),
            t(Lte),
            t(Gt),
            t(Gte),
            t(Div),
            t(Mod),
            t(Minus),
            t(Plus),
            t(Slash),
            t(Star),
            t(If),
        )),
    )(input)
}

// Ebnf group ExpressionByNamesRules

// Ebnf group ConstantRules

pub fn parse_constant<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(ConstantNode, alt((t(True), t(False), t(IConst), t(RConst))))(input)
}

/// Actual parsing rules
#[allow(dead_code)]
impl<'a> Parser<'a> {
    fn equation(&mut self) -> bool {
        let equation = self.builder.checkpoint();

        if self.accept(Assert) || (self.accept_left() && self.accept(Equal)) {
            self.builder
                .start_node_at(equation, EqualsEquationNode.into());
        } else {
            return false;
        }

        self.expression();
        self.expect(Semicolon);

        self.end();
        true
    }

    fn equation_list(&mut self) {
        if !self.equation() {
            self.error_until(
                "Expected at least one expression in the node's body",
                &[Tel],
            );
        }

        while self.equation() {}
    }

    fn expression(&mut self) -> bool {
        // FIXME: this is an extremely stupid and lax implementation
        const TOKENS: &[Token] = &[
            True, False, IConst, RConst, Ident, Not, Minus, Pre, Current, Int, Real, When, FBy,
            Arrow, And, Or, Xor, Impl, Neq, Equal, Lt, Lte, Gt, Gte, Div, Mod, Plus, Slash, Star,
            If, Then, Else, With, Nor, Sharp, Nor, Hat, Bar, Dot, Merge, CDots,
        ];

        self.start(ExpressionNode);
        while {
            self.skip_trivia();
            match self.current() {
                Some(d @ OpenPar | d @ OpenBracket) => {
                    self.start(ExpressionNode);
                    self.next();
                    self.expression();
                    match d {
                        OpenPar => self.expect(ClosePar),
                        OpenBracket => self.expect(CloseBracket),
                        _ => unreachable!(),
                    };
                    self.end();
                    true
                }
                token if TOKENS.contains(&token.unwrap_or(Semicolon)) => {
                    self.next();
                    true
                }
                _ => false,
            }
        } {}
        self.end();
        true
    }

    fn id_ref(&mut self) {
        // TODO that's not how it works
        //   c.f.: https://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lv6-ref-man.pdf#Lv6IdRef
        self.expect(Ident);
    }

    fn accept_lv6_id(&mut self) -> bool {
        // FIXME
        self.accept(Ident)
    }

    fn toplevel_decl(&mut self) -> bool {
        self.skip_trivia();

        match self.current() {
            Some(Model) | Some(Package) => self.package_list(),
            Some(_) => self.package_body(),
            None => return false,
        }
        true
    }

    fn package_list(&mut self) {
        self.start(PackageList);
        loop {
            self.skip_trivia();
            match self.current() {
                Some(Model) => {
                    self.model_decl();
                }
                Some(Package) => {
                    if self.peek() == Some([Equal]) || self.peek() == Some([Is]) {
                        self.package_eq();
                    } else {
                        self.package_decl();
                    }
                }
                Some(_) => {
                    self.error_until("Expected a package or model declaration", &[Model, Package])
                }
                None => {
                    self.end();
                    return;
                }
            }
        }
    }

    fn package_body(&mut self) {
        self.start(PackageBody);
        loop {
            self.skip_trivia();
            match self.current() {
                Some(Const) => {
                    self.const_decls();
                }
                Some(Node) | Some(Unsafe) | Some(Extern) | Some(Function) => self.node_decl(),
                Some(Type) => self.type_decls(),
                Some(End) | None => break,
                _ => self.error_until("Expected a declaration", NEW_DECL),
            }
        }
        self.end();
    }

    fn model_decl(&mut self) {
        self.start(ModelDecl);
        self.expect(Model);
        self.ident();
        self.uses();
        self.expect(Needs);
        self.accept_static_params();
        self.provides();
        self.expect(Body);
        self.package_body();
        self.expect(End);
        self.end();
    }

    fn package_eq(&mut self) {
        self.error("TODO: package alias")
    }

    fn package_decl(&mut self) {
        self.error("TODO: package declaration")
    }

    fn ident(&mut self) -> bool {
        // TODO: qualified idents
        self.expect(Ident)
    }

    fn uses(&mut self) {
        self.error("TODO: uses")
    }

    fn provides(&mut self) {
        self.error("TODO: provides")
    }

    fn const_decls(&mut self) -> bool {
        self.error("TODO: const decls");
        false
    }

    fn accept_left(&mut self) -> bool {
        // FIXME
        self.accept(Ident)
    }

    fn node_decl(&mut self) {
        self.start(NodeDecl);
        self.accept(Unsafe);
        self.accept(Extern);
        if self.current() != Some(Node) && self.current() != Some(Function) {
            self.error_until("Expected node or function keyword", NEW_DECL);
        }
        self.next();
        self.ident();
        self.skip_trivia();
        self.accept_static_params();
        self.skip_trivia();
        if self.current() == Some(OpenPar) {
            self.params();
            self.accept_returns();
        }

        self.skip_trivia();
        match self.current() {
            // external or normal node
            Some(Semicolon) => {
                self.next();
                while self.var_decls() || self.const_decls() {}
                self.skip_trivia();
                if self.current() == Some(Let) {
                    self.node_body();
                }
            }
            // alias node
            Some(Equal) => {
                self.next();
                self.effective_node();
                self.accept(Semicolon);
            }
            // node definition
            Some(Let) => {
                self.node_body();
            }
            _ => self.error_until("Expected a semicolon or a node alias", NEW_DECL),
        }

        self.end();
    }

    fn expect_type(&mut self) -> bool {
        self.skip_trivia();

        // TODO: maybe don't start a node before being sure it can be parsed
        self.start(TypeNode);

        match self.current() {
            Some(Bool) => self.next(),
            Some(Int) => self.next(),
            Some(Real) => self.next(),
            Some(Ident) => self.id_ref(), // FIXME: pattern is wrong, it should match any "IdRef"
            _ => {
                self.error_until("Not a type", &[Colon, Semicolon, Hat]);
                self.end();
                return false;
            }
        }

        if self.accept(Hat) {
            self.expression();
        }

        self.end();

        true
    }

    fn type_decls(&mut self) {
        self.next()
    }

    fn accept_typed_lv6_ids(&mut self) -> bool {
        if !self.accept_lv6_id() {
            return false;
        }

        while self.accept(Comma) {
            self.expect(Ident);
        }

        if self.accept(Colon) {
            self.expect_type();
        } else {
            self.error_until("Missing type", &[Semicolon, ClosePar]);
        }

        true
    }

    fn params(&mut self) {
        self.start(ParamsDecl);
        self.expect(OpenPar);
        self.var_decl_list();
        self.expect(ClosePar);
        self.end();
    }

    fn node_body(&mut self) {
        self.expect(Let);
        self.equation_list();
        self.expect(Tel);
        self.accept(Semicolon);
    }

    fn accept_var_decl(&mut self) -> bool {
        self.start(VarDecl);
        let success = self.accept_typed_lv6_ids();
        // FIXME: also handle clock/when expressions
        self.end();
        success
    }

    fn var_decl_list(&mut self) {
        if !self.accept_var_decl() {
            self.error_until("Expected at least one declaration", &[ClosePar]);
            return;
        }

        self.skip_trivia();
        while let Some([Semicolon, Ident]) = self.peek() {
            self.expect(Semicolon);
            self.accept_var_decl();
        }
    }

    fn var_decls(&mut self) -> bool {
        self.error("TODO: var decls");
        false
    }

    fn accept_returns(&mut self) {
        self.skip_trivia();
        if self.current() == Some(Returns) {
            self.start(ReturnsNode);
            self.next();
            self.params();
            self.accept(Semicolon);
            self.end();
        }
    }

    // Ebnf group StaticRules

    fn accept_static_params(&mut self) -> bool {
        if self.current() == Some(OpenStaticPar) {
            self.start(StaticParamsNode);

            while {
                self.skip_trivia();
                self.current() != Some(CloseStaticPar)
            } {
                self.next();
                if let Err(msg) = self.static_param() {
                    self.error(msg);
                }
            }

            self.next();
            self.end();
            true
        } else {
            false
        }
    }

    fn static_param(&mut self) -> Result<(), &'static str> {
        fn function_or_node_end(s: &mut Parser) {
            s.accept_lv6_id();
            s.params();
            s.expect(Returns);
            s.params();
        }

        self.skip_trivia();
        match self.current() {
            Some(Type) => {
                self.next();
                self.accept_lv6_id();
                Ok(())
            }
            Some(Const) => {
                self.next();
                self.accept_lv6_id();
                if self.accept(Colon) {
                    self.expect_type();
                } else {
                    self.error("Expected colon and type");
                }
                Ok(())
            }
            Some(Node) | Some(Function) => {
                function_or_node_end(self);
                Ok(())
            }
            Some(Unsafe) => {
                self.next();
                self.skip_trivia();
                if let Some(Node) | Some(Function) = self.current() {
                    function_or_node_end(self);
                } else {
                    self.error("Expected `node` or `function` after `unsafe`")
                }

                Ok(())
            }
            _ => Err(
                "Expected `type`, `const`, `node`, `function`, `unsafe node` or `unsafe function`",
            ),
        }
    }

    fn effective_node(&mut self) {
        self.id_ref();
        self.static_arg_list();
    }

    fn static_arg_list(&mut self) {
        if self.current() == Some(OpenStaticPar) {
            self.start(StaticArgsNode);

            while {
                self.skip_trivia();
                self.current() != Some(CloseStaticPar)
            } {
                self.next();
                if let Err(msg) = self.static_arg() {
                    self.error(msg);
                }
            }

            self.next(); // >>
            self.end();
        }
    }

    fn static_arg(&mut self) -> Result<(), &'static str> {
        self.skip_trivia();
        match self.current() {
            Some(Type) => {
                self.start(StaticArgNode);
                self.next();
                self.expect_type();
                self.end();
                Ok(())
            }
            Some(Const) => {
                self.start(StaticArgNode);
                self.next();
                self.expression();
                self.end();
                Ok(())
            }
            Some(Node) | Some(Function) => {
                self.start(StaticArgNode);
                self.next();
                self.effective_node();
                self.end();
                Ok(())
            }
            // TODO PredefOp
            Some(IConst) => {
                // FIXME: SimpleExp (or simply expression and we check at a later stage)
                self.start(StaticArgNode);
                self.start(ExpressionNode);
                self.next();
                self.end();
                self.end();
                Ok(())
            }
            // TODO SurelyType
            // TODO SurelyNode
            _ => Err("Expected `type`, `const`, `node`, `function` or TODO"),
        }
    }
}
