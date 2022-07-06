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
        loop {
            match self.current() {
                Some(Comment) | Some(InlineComment) | Some(Space) => self.next(),
                _ => break,
            }
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

        loop {
            if let Some(curr) = self.current() {
                if stop.contains(&curr) {
                    break;
                }
                self.next();
            } else {
                break;
            }
        }

        self.errors.push(super::Error {
            msg: msg.to_owned(),
            span: start..self.end_pos(),
        });
        self.end();
    }

    fn peek<const N: usize>(&self) -> [Token; N] {
        let start = self.tokens.len() - 1 - N;
        self.tokens
            .iter()
            .map(|(t, _)| *t)
            .skip(start)
            .collect::<Vec<_>>()[..]
            .try_into()
            .unwrap()
    }

    /// Report an error if the current token is not the expected one,
    /// moves to the next token if it matched
    fn expect(&mut self, expected: Token) {
        self.skip_trivia();
        let current = self.current();
        if current != Some(expected) {
            self.error(format!(
                "Unexpected token: {:?} (expected {:?})",
                current, expected
            ));
        } else {
            self.next()
        }
    }

    /// Advance only if the next token is the given one
    ///
    /// Allows for optionally matching a token
    fn accept(&mut self, tok: Token) {
        self.skip_trivia();
        if self.current() == Some(tok) {
            self.next();
        }
    }

    fn start_pos(&self) -> usize {
        self.pos
    }

    fn end_pos(&self) -> usize {
        self.pos + self.tokens.last().map(|x| x.1.len()).unwrap_or(0)
    }
}

static NEW_DECL: &'static [Token] = &[Const, Type, Node, Unsafe, Extern, Function, End];

/// Actual parsing rules
impl<'a> Parser<'a> {
    pub fn file(&mut self) {
        self.start(Root);

        while self.include_statement() {}
        while self.toplevel_decl() {}

        self.end();
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
                    if self.peek() == [Equal] || self.peek() == [Is] {
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
        self.static_params_decl();
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

    fn ident(&mut self) {
        // TODO: qualified idents
        self.expect(Ident);
    }

    fn uses(&mut self) {
        self.error("TODO: uses")
    }

    fn static_params_decl(&mut self) {
        self.error("TODO: static params decl")
    }

    fn provides(&mut self) {
        self.error("TODO: provides")
    }

    fn const_decls(&mut self) -> bool {
        self.error("TODO: const decls");
        false
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
        if self.current() == Some(OpenStaticPar) {
            self.paren_static_param_decl();
        }
        self.skip_trivia();
        if self.current() == Some(OpenPar) {
            self.param_decl();
            self.returns();
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
                self.effective_node();
                self.accept(Semicolon); // TODO: isn't it mandatory?
            }
            _ => self.error_until("Expected a semicolon or a node alias", NEW_DECL),
        }

        self.end();
    }

    fn type_decls(&mut self) {
        self.next()
    }

    /// Static parameters declaration, with surrrounding "static parenthesis" (<< and >>).
    ///
    /// Self::static_param_decl only parses the inner list of paramater, because it is useful
    /// in other places, where there are no << >>
    fn paren_static_param_decl(&mut self) {
        self.expect(OpenStaticPar);
        self.static_params_decl();
        self.expect(CloseStaticPar);
    }

    fn param_decl(&mut self) {
        self.start(ParamDecl);
        self.expect(OpenPar);
        self.error("TODO: param decl");
        self.end();
    }

    fn node_body(&mut self) {
        self.expect(Let);
        self.error("TODO: node body");
        self.expect(Tel);
        self.accept(Semicolon);
    }

    fn var_decls(&mut self) -> bool {
        self.error("TODO: var decls");
        false
    }

    fn returns(&mut self) {
        self.error("TODO: returns")
    }

    fn effective_node(&mut self) {
        self.error("TODO: effective node")
    }
}
