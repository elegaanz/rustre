use crate::lexer::{Tok, TokInfo};
use crate::location::Spanned;

pub struct Parser<'a, 'f> {
    pub errors: Vec<Error<'a, 'f>>,
}

#[derive(Debug)]
pub enum Error<'a, 'f> {
    UnexpectedToken(&'a [Tok<'a, 'f>], &'static str),
}

#[derive(Debug)]
pub struct Ast<'a, 'f> {
    includes: Vec<Spanned<'f, &'a str>>,
    decls: Vec<Spanned<'f, Decl<'a, 'f>>>,
}

#[derive(Debug)]
pub enum Decl<'a, 'f> {
    Const {
        name: &'a str,
        /// May be None for external constants
        value: Option<Spanned<'f, Expr>>,
        ty: Option<&'a str>,
    },
    Ty,
    ExtNode,
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: &'a str,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        vars: Vec<()>,
        body: Vec<()>,
    },
    AliasNode {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, &'a str>,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        effective_node: Spanned<'f, (Spanned<'f, &'a str>, Vec<Spanned<'f, StaticParam<'a, 'f>>>)>,
    },
}

#[derive(Debug)]
pub enum StaticParamDecl<'a, 'f> {
    Const {
        name: &'a str,
        ty: &'a str,
    },
    Ty {
        name: &'a str,
    },
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, &'a str>,
        params: Vec<Spanned<'f, Variable<'a, 'f>>>,
        outputs: Vec<Spanned<'f, Variable<'a, 'f>>>,
    },
}

#[derive(Clone, Debug)]
pub struct StaticParam<'a, 'f> {
    _name: &'a str,
    _name2: &'f str,
}
#[derive(Clone, Debug)]
pub struct Variable<'a, 'f> {
    _name: &'a str,
    _name2: &'f str,
}
#[derive(Clone, Debug)]
pub struct VariableDecl<'a, 'f> {
    _name: &'a str,
    _name2: &'f str,
}

#[derive(Clone, Debug)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Const(ConstValue),
}

type Res<'a, 'f, T> = Result<(&'a [Tok<'a, 'f>], T), Error<'a, 'f>>;
type SpannedRes<'a, 'f, T> = Res<'a, 'f, Spanned<'f, T>>;

impl<'a, 'f> Parser<'a, 'f> {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    fn report<T>(&mut self, res: Result<T, Error<'a, 'f>>) -> Option<T> {
        match res {
            Ok(x) => Some(x),
            Err(e) => {
                self.errors.push(e);
                None
            }
        }
    }

    fn expect(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        t: TokInfo<'a>,
        exp: &'static str,
    ) -> Result<&'a Tok<'a, 'f>, Error<'a, 'f>> {
        match toks.get(0) {
            Some(x) if x.item == t => Ok(x),
            _ => Err(Error::UnexpectedToken(toks, exp)),
        }
    }

    pub fn parse(&mut self, toks: &'a [Tok<'a, 'f>]) -> Result<Ast<'a, 'f>, Error<'a, 'f>> {
        let mut includes = vec![];
        let mut toks = toks;
        while let Ok((t, inc)) = self.parse_include(toks) {
            includes.push(inc);
            toks = t;
        }

        let decls = if let Ok((t, decls)) = self.parse_package_body(toks) {
            toks = t;
            decls
        } else if let Ok((t, decls)) = self.parse_package_list(toks) {
            toks = t;
            decls
        } else {
            return Err(Error::UnexpectedToken(toks, "expected declarations"));
        };

        self.expect(toks, TokInfo::EOF, "expected end of file")?;

        Ok(Ast { includes, decls })
    }

    fn parse_include(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, &'a str> {
        if let Ok(kw) = self.expect(toks, TokInfo::Include, "expected include") {
            match toks.get(1) {
                Some(
                    tok
                    @
                    Spanned {
                        item: TokInfo::Str(inc),
                        ..
                    },
                ) => Ok((&toks[2..], Spanned::fusion(kw, tok, inc))),
                _ => Err(Error::UnexpectedToken(
                    &toks[1..],
                    "expected a file name between quotes",
                )),
            }
        } else {
            Err(Error::UnexpectedToken(toks, "expected include"))
        }
    }

    fn parse_package_list(
        &mut self,
        _toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        todo!()
    }

    // parses many times the same thing
    // if an error is found it is reported and the tokens are skipped
    // until another item can be parsed
    fn parse_many<F, E, T>(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        sep: Option<TokInfo<'a>>,
        end: E,
        parse: F,
    ) -> Res<'a, 'f, Vec<T>>
    where
        F: Fn(&mut Self, &'a [Tok<'a, 'f>]) -> Res<'a, 'f, Vec<T>>,
        E: Fn(&mut Self, &'a [Tok<'a, 'f>]) -> bool,
    {
        let mut items = Vec::new();
        let mut toks = toks;
        let mut err = None;
        let mut first = true;
        while toks.len() > 1 && !end(self, toks) {
            if first {
                first = false;
            } else {
                if let Some(s) = sep.clone() {
                    self.expect(toks, s, "separator")?;
                    toks = &toks[1..];
                }
            }

            match parse(self, toks) {
                Ok((t, mut item)) => {
                    if let Some(e) = err.take() {
                        self.report::<()>(Err(e));
                    }
                    items.append(&mut item);
                    toks = t;
                }
                Err(e) => {
                    if err.is_none() {
                        err = Some(e);
                    }
                    if toks.len() > 1 && !end(self, toks) {
                        toks = &toks[1..];
                    }
                }
            }
        }

        if let Some(e) = err.take() {
            self.report::<()>(Err(e));
        }

        Ok((toks, items))
    }

    fn parse_package_body(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        self.parse_many(
            toks,
            None,
            |_, _| false,
            |s, t| {
                s.parse_const_decl(t)
                    .or_else(|_| s.parse_local_node_decl(t))
            },
        )
    }

    fn parse_const_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        let kw = self.expect(toks, TokInfo::Const, "expected const keyword")?;
        let mut toks = toks;
        let (t, id) = self.parse_id(&toks[1..])?;
        toks = t;
        let mut ids = vec![id];
        while self.expect(toks, TokInfo::Coma, "expected ,").is_ok() {
            let (t, id) = self.parse_id(&toks[1..])?;
            ids.push(id);
            toks = t;
        }

        let ty = if self.expect(toks, TokInfo::Colon, "expected :").is_ok() {
            let (t, id) = self.parse_id(&toks[1..])?;
            toks = t;
            Some(id)
        } else {
            None
        };

        let expr = if ids.len() == 1 && self.expect(toks, TokInfo::Equal, "expected =").is_ok() {
            let (t, expr) = self.parse_expr(&toks[1..])?;
            toks = t;
            Some(expr)
        } else {
            None
        };

        self.expect(toks, TokInfo::Semicolon, "expected ;")?;

        let ty = ty.map(|x| x.item);
        Ok((
            &toks[1..],
            ids.into_iter()
                .map(|i| {
                    Spanned::fusion(
                        kw,
                        &i,
                        Decl::Const {
                            name: i.item,
                            ty,
                            value: expr.clone(),
                        },
                    )
                })
                .collect(),
        ))
    }

    fn parse_id(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, &'a str> {
        match toks.get(0) {
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::Ident(x),
                    ..
                },
            ) => Ok((
                &toks[1..],
                Spanned {
                    span: tok.span.clone(),
                    item: x,
                },
            )),
            _ => Err(Error::UnexpectedToken(&toks, "expected an identifier")),
        }
    }

    fn parse_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr> {
        let cst = self.parse_const(toks);
        cst
    }

    fn parse_const(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr> {
        let neg = self.expect(toks, TokInfo::Minus, "expected -").is_ok();
        match toks.get(if neg { 1 } else { 0 }) {
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::IConst(i),
                    ..
                },
            ) => Ok((
                &toks[1..],
                Spanned {
                    span: tok.span.clone(),
                    item: Expr::Const(ConstValue::Int(if neg { -i } else { *i })),
                },
            )),
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::RConst(r),
                    ..
                },
            ) => Ok((
                &toks[1..],
                Spanned {
                    span: tok.span.clone(),
                    item: Expr::Const(ConstValue::Float(if neg { -r } else { *r })),
                },
            )),
            _ => Err(Error::UnexpectedToken(&toks, "expected a constant")),
        }
    }

    fn parse_local_node_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        let start_span = toks[0].span.clone();
        let mut toks = toks;
        let is_unsafe = self.expect(toks, TokInfo::Unsafe, "unsafe keyword").is_ok();
        toks = if is_unsafe { &toks[1..] } else { toks };
        let is_node = self.expect(&toks, TokInfo::Node, "node keyword").is_ok();
        if !is_node {
            self.expect(&toks, TokInfo::Function, "function or node keyword")?;
        }

        toks = &toks[1..];
        let (t, name) = self.parse_id(&toks)?;
        toks = t;
        let static_params = if self.expect(&toks, TokInfo::OpenStaticPar, "<<").is_ok() {
            let (t, sp) = self.parse_many(
                toks,
                Some(TokInfo::Semicolon),
                |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                |s, t| s.parse_static_param_decl(t),
            )?;
            toks = t;
            self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
            sp
        } else {
            Vec::new()
        };

        let has_params = self.expect(toks, TokInfo::OpenPar, "(").is_ok();
        let params = if has_params {
            let (t, pars) = self.parse_var_decl(toks)?;
            toks = t;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            pars
        } else {
            Vec::new()
        };

        let returns = if has_params {
            self.expect(toks, TokInfo::Returns, "expected returns")?;
            let (t, rets) = self.parse_var_decl(toks)?;
            toks = t;
            rets
        } else {
            Vec::new()
        };

        let is_alias = self.expect(toks, TokInfo::Equal, "=").is_ok();
        if !is_alias && !has_params {
            return Err(Error::UnexpectedToken(
                toks,
                "expected a node alias (declared with a =) or parameters",
            ));
        }

        if is_alias {
            toks = &toks[1..];
            let name_span = toks[0].span.clone();
            let (t, effective_node_name) = self.parse_id(toks)?;
            toks = t;
            let effective_static_params = if self
                .expect(&toks[1..], TokInfo::OpenStaticPar, ">>")
                .is_ok()
            {
                // TODO: the separator may be a ; too
                let (t, params) = self.parse_many(
                    toks,
                    Some(TokInfo::Coma),
                    |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                    |s, t| s.parse_static_param(t),
                )?;
                toks = t;
                self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
                params
            } else {
                Vec::new()
            };
            toks = if self.expect(toks, TokInfo::Semicolon, ";").is_ok() {
                &toks[1..]
            } else {
                toks
            };

            let effective_node = Spanned::fusion(
                name_span,
                toks[0].span.clone(),
                (effective_node_name, effective_static_params),
            );

            Ok((
                toks,
                vec![Spanned::fusion(
                    start_span,
                    toks[0].span.clone(),
                    Decl::AliasNode {
                        name,
                        params,
                        outputs: returns,
                        is_unsafe,
                        is_function: !is_node,
                        effective_node,
                        static_params,
                    },
                )],
            ))
        } else {
            todo!()
        }
    }

    fn parse_static_param(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, StaticParam<'a, 'f>>>> {
        Ok((toks, vec![]))
    }

    fn parse_var_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, VariableDecl<'a, 'f>>>> {
        Ok((toks, vec![]))
    }
    fn parse_static_param_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>> {
        Ok((toks, vec![]))
    }
}
