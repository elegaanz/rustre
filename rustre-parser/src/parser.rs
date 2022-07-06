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
}

/// Utils
impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<(Token, &'a str)>) -> Parse {
        let mut parser = Parser {
            tokens,
            errors: Vec::new(),
            builder: GreenNodeBuilder::new(),
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
            self.builder.token(tok.into(), source);
        }
    }

    /// Reports an error and skips one token
    fn error(&mut self, msg: &str) {
        self.start(Error);
        self.errors.push(msg.to_owned());
        self.next();
        self.end();
    }

    fn error_until(&mut self, msg: &str, stop: &[Token]) {
        self.start(Error);
        self.errors.push(msg.to_owned());
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
        if self.current() != Some(expected) {
            self.error("Unexpected token");
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
}

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
                    Some(_) => self.error("unexpected token"),
                    None => self.error("unexpected end of file"),
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
                Some(_) => self.error_until("unexpected token", &[Model, Package]),
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
                Some(Const) => self.const_decls(),
                Some(Node) | Some(Unsafe) | Some(Extern) | Some(Function) => self.node_decl(),
                Some(Type) => self.type_decls(),
                Some(End) | None => break,
                _ => self.error_until("unexpected token", &[Const, Node, Type, End]),
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
        self.next()
    }

    fn package_decl(&mut self) {
        self.next()
    }

    fn ident(&mut self) {
        // TODO: qualified idents
        self.expect(Ident);
    }

    fn uses(&mut self) {
        self.next()
    }

    fn static_params_decl(&mut self) {
        self.next()
    }

    fn provides(&mut self) {
        self.next()
    }

    fn const_decls(&mut self) {
        self.next()
    }

    fn node_decl(&mut self) {
        self.next()
    }

    fn type_decls(&mut self) {
        self.next()
    }
}
