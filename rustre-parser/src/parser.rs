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
        self.next() // TODO
    }

    fn package_body(&mut self) {
        self.next() // TODO
    }
}
