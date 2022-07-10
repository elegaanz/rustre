use rowan::GreenNodeBuilder;

use crate::{
    lexer::Token::{self, *},
    Parse, SyntaxNode,
};

pub struct Parser<'a> {
    /// Stack of remaining tokens
    ///
    /// The first token is at the end of the Vec (top of the stack)
    tokens: Vec<(Token, &'a str)>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<super::Error>,
    pos: usize,
}

/// Utils
impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<(Token, &'a str)>) -> Parse {
        let mut parser = Parser {
            tokens,
            errors: Vec::new(),
            builder: GreenNodeBuilder::new(),
            pos: 0,
        };

        parser.file();

        Parse {
            root: SyntaxNode::new_root(parser.builder.finish()),
            errors: parser.errors,
        }
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
        while let Some(Comment) | Some(InlineComment) | Some(Space) = self.current() {
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

/// Actual parsing rules
impl<'a> Parser<'a> {
    fn equation(&mut self) -> bool {
        let equation = self.builder.checkpoint();

        if self.accept(Assert) || (self.accept_left() && self.accept(Equal)) {
            self.builder.start_node_at(equation, EquationNode.into());
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

    fn expression(&mut self) {
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
    }

    pub fn file(&mut self) {
        self.start(Root);

        while self.include_statement() {}
        while self.toplevel_decl() {}

        self.end();
    }

    fn id_ref(&mut self) {
        // TODO that's not how it works
        //   c.f.: https://www-verimag.imag.fr/DIST-TOOLS/SYNCHRONE/lustre-v6/doc/lv6-ref-man.pdf#Lv6IdRef
        self.expect(Ident);
    }

    /// Returns true while it can parse
    fn include_statement(&mut self) -> bool {
        self.skip_trivia();
        match self.current() {
            Some(Include) => {
                self.start(IncludeStatement);
                self.next();
                self.skip_trivia();
                match self.current() {
                    Some(Str) => self.next(),
                    Some(_) => self.error("Unexpected token: expected a string literal"),
                    None => self.error("Unexpected end of file"),
                }
                self.end();
                true
            }
            _ => false,
        }
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
