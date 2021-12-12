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
    decls: Vec<Spanned<'f, Decl<'a>>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Const {
        name: &'a str,
        /// May be None for external constants
        value: Option<Expr>,
        ty: Option<&'a str>,
    },
    Ty,
    ExtNode,
    Node,
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
                _ => Err(Error::UnexpectedToken(&toks[1..], "expected a file name between quotes")),
            }
        } else {
            Err(Error::UnexpectedToken(toks, "expected include"))
        }
    }

    fn parse_package_list(
        &mut self,
        _toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
        todo!()
    }

    fn parse_package_body(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
        let mut decls = Vec::new();
        let mut toks = toks;
        while toks.len() > 1 {
            while let Ok((t, mut decl)) = self.parse_const_decl(toks) {
                decls.append(&mut decl);
                toks = t;
            }
            if toks.len() > 1 {
                let err = self.parse_const_decl(toks);
                toks = &toks[1..];
                'skip_err: while let Err(_) = self.parse_const_decl(toks) {
                    if toks.len() <= 1 {
                        break 'skip_err;
                    }
                    toks = &toks[1..];
                }
                self.report(err);
            }
        }
        Ok((toks, decls))
    }

    fn parse_const_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
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
        let value = expr.map(|x| x.item);
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
                            value: value.clone(),
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
}
