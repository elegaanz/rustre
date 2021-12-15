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
        value: Option<Spanned<'f, Expr<'a, 'f>>>,
        ty: Option<&'a str>,
    },
    Ty,
    ExtNode,
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, &'a str>,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        vars: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        body: Vec<Spanned<'f, BodyItem<'a, 'f>>>,
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
        name: Spanned<'f, &'a str>,
        ty: Spanned<'f, Ty<'a, 'f>>,
    },
    Ty {
        name: Spanned<'f, &'a str>,
    },
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, &'a str>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
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
    name: Spanned<'f, &'a str>,
    ty: Spanned<'f, Ty<'a, 'f>>,
}

#[derive(Clone, Debug)]
pub struct Ty<'a, 'f> {
    len: Option<Spanned<'f, Expr<'a, 'f>>>,
    base: BaseTy,
}

#[derive(Clone, Debug)]
pub enum BaseTy {
    Bool,
    Int,
    Real,
}

#[derive(Clone, Debug)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub enum Expr<'a, 'f> {
    Const(ConstValue),
    Ident(&'a str),
    Unary(Spanned<'f, UnaryOp>, Spanned<'f, Box<Expr<'a, 'f>>>),
    Binary(
        Spanned<'f, BinaryOp>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
    ),
    StructAccess(Spanned<'f, Box<Expr<'a, 'f>>>, &'a str),
    NamedClock(Spanned<'f, &'a str>, Box<Option<Spanned<'f, &'a str>>>),
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Not,
    Minus,
    Pre,
    Current,
    IntToReal,
    RealToInt,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    When,
    FBy,
    Arrow,
    And,
    Or,
    Xor,
    Impl,
    Equal,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Mod,
    Minus,
    Plus,
    Slash,
    Hat,
    Bar,
    Index,
    Slice,
    Div,
    Prod,
}

#[derive(Debug)]
pub enum BodyItem<'a, 'f> {
    Assert(Spanned<'f, Expr<'a, 'f>>),
    Equation(
        Vec<Spanned<'f, LeftItem<'a, 'f>>>,
        Spanned<'f, Expr<'a, 'f>>,
    ),
}

#[derive(Debug)]
pub enum LeftItem<'a, 'f> {
    Ident(Spanned<'f, &'a str>),
    Field(Box<Spanned<'f, LeftItem<'a, 'f>>>, Spanned<'f, &'a str>),
    TableIndex(
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        Spanned<'f, Expr<'a, 'f>>,
    ),
    TableSlice(
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        Spanned<'f, Expr<'a, 'f>>,
        Spanned<'f, Expr<'a, 'f>>,
    ),
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
                    self.expect(toks, s, "expected separator")?;
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
        // TODO: long ids
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

    fn parse_unary(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        tok: TokInfo<'a>,
        op: UnaryOp,
    ) -> Result<Spanned<'f, Expr<'a, 'f>>, Error<'a, 'f>> {
        let op_token = self.expect(toks, tok, "expected unary operator")?;
        let expr = self.parse_expr(&toks[1..])?;
        Ok(Spanned::fusion(
            toks[0].span.clone(),
            expr.span.clone(),
            Expr::Unary(
                Spanned {
                    item: op,
                    span: op_token.span.clone(),
                },
                Spanned {
                    span: expr.span,
                    item: Box::new(expr.item),
                },
            ),
        ))
    }

    fn parse_binary(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        tok_op: TokInfo<'a>,
        op: BinaryOp,
    ) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        self.parse_binary_custom(toks, tok_op, op, Self::parse_expr, Self::parse_expr)
    }

    fn parse_binary_custom<L, R>(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        tok_op: TokInfo<'a>,
        op: BinaryOp,
        lhs_parser: L,
        rhs_parser: R,
    ) -> SpannedRes<'a, 'f, Expr<'a, 'f>>
    where
        L: Fn(&mut Self, &'a [Tok<'a, 'f>]) -> Result<Spanned<'f, Expr<'a, 'f>>, Error<'a, 'f>>,
        R: Fn(&mut Self, &'a [Tok<'a, 'f>]) -> Result<Spanned<'f, Expr<'a, 'f>>, Error<'a, 'f>>,
    {
        let mut op_pos = None;
        for (i, tok) in toks.iter().enumerate() {
            if tok.item == tok_op {
                op_pos = Some(i);
                break;
            }
        }

        if let Some(op_pos) = op_pos {
            let before = &toks[0..op_pos];
            let after = &toks[op_pos + 1..];
            let lhs = lhs_parser(self, before)?;
            let rhs = rhs_parser(self, after)?;
            Ok((
                toks,
                Spanned::fusion(
                    toks[0].span.clone(),
                    toks[toks.len() - 1].span.clone(),
                    Expr::Binary(
                        Spanned {
                            item: op,
                            span: toks[op_pos].span.clone(),
                        },
                        Spanned {
                            span: lhs.span,
                            item: Box::new(lhs.item),
                        },
                        Spanned {
                            span: rhs.span,
                            item: Box::new(rhs.item),
                        },
                    ),
                ),
            ))
        } else {
            Err(Error::UnexpectedToken(toks, "expected binary operator"))
        }
    }

    fn parse_clock_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = toks[0].span.clone();
        let not = self.expect(toks, TokInfo::Not, "not").is_ok();
        let toks = if not { &toks[1..] } else { toks };
        let (toks, name) = self.parse_id(toks)?;
        let (toks, param) = if self.expect(toks, TokInfo::OpenPar, "(").is_ok() {
            let (toks, param) = self.parse_id(&toks[1..])?;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            (toks, Some(param))
        } else {
            (toks, None)
        };
        Ok((
            toks,
            Spanned::fusion(
                start,
                toks[0].span.clone(),
                Expr::NamedClock(name, Box::new(param)),
            ),
        ))
    }

    // toks is not a slice from the current position to the end of the file,
    // but from the current pos to the end of the expression to parse
    fn parse_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> Result<Spanned<'f, Expr<'a, 'f>>, Error> {
        if toks.len() == 0 {
            panic!("should stop recursing before");
        }
        // TODO
        // - if EXPR then EXPR else EXPR
        // - with EXPR then EXPR else EXPR
        // - #(EXPR , EXPR)
        // - nor (EXPR , EXPR)
        // - [ EXPR, EXPR, EXPR ]
        // - EXPR [ EXPR ]
        // - EXPR [ EXPR .. EXPR ]
        // - EXPR [ EXPR .. EXPR step EXPR ]
        // - EXPR . ID
        // - (EXPR)
        // - merge ID MERGE_CASES
        // - CALL_BY_POS
        // - CALL_BY_NAME
        let const_expr = self.parse_const(toks);
        let id = self.parse_id(toks).map(|(t, id)| {
            (
                t,
                Spanned {
                    span: id.span.clone(),
                    item: Expr::Ident(id.item),
                },
            )
        });
        if toks.len() == 1 {
            const_expr
                .or(id)
                .map_err(|_| Error::UnexpectedToken(toks, "expected expression"))
                .map(|(_, x)| x)
        } else {
            const_expr
                .or(id)
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Not, UnaryOp::Not))
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Minus, UnaryOp::Minus))
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Pre, UnaryOp::Pre))
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Current, UnaryOp::Current))
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Int, UnaryOp::RealToInt))
                .or_else(|_: Error| self.parse_unary(toks, TokInfo::Real, UnaryOp::IntToReal))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::FBy, BinaryOp::FBy))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Arrow, BinaryOp::Arrow))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::And, BinaryOp::And))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Or, BinaryOp::Or))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Xor, BinaryOp::Xor))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Impl, BinaryOp::Impl))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Equal, BinaryOp::Equal))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Neq, BinaryOp::Neq))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Lt, BinaryOp::Lt))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Lte, BinaryOp::Lte))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Gt, BinaryOp::Gt))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Gt, BinaryOp::Gte))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Div, BinaryOp::Div))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Mod, BinaryOp::Mod))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Minus, BinaryOp::Minus))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Plus, BinaryOp::Plus))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Slash, BinaryOp::Slash))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Star, BinaryOp::Prod))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Bar, BinaryOp::Bar))
                .or_else(|_: Error| self.parse_binary(toks, TokInfo::Hat, BinaryOp::Hat))
                .or_else(|_| {
                    let lhs = self.parse_expr(&toks)?;
                    let op_token = self.expect(t, TokInfo::When, "expected binary operator")?;
                    let rhs = self.parse_clock_expr(&t[1..])?;
                    Ok((
                        t,
                        Spanned::fusion(
                            toks[0].span.clone(),
                            t[0].span.clone(),
                            Expr::Binary(
                                Spanned {
                                    item: BinaryOp::When,
                                    span: op_token.span.clone(),
                                },
                                Spanned {
                                    span: lhs.span,
                                    item: Box::new(lhs.item),
                                },
                                Spanned {
                                    span: rhs.span,
                                    item: Box::new(rhs.item),
                                },
                            ),
                        ),
                    ))
                })
                .map(|(_, x)| x)
        }
    }

    fn parse_const(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
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
            self.expect(
                &toks,
                TokInfo::Function,
                "expected function or node keyword",
            )?;
        }

        toks = &toks[1..];
        let (t, name) = self.parse_id(&toks)?;
        toks = t;
        let static_params = if self.expect(&toks, TokInfo::OpenStaticPar, "<<").is_ok() {
            let (t, sp) = self.parse_many(
                &toks[1..],
                Some(TokInfo::Semicolon),
                |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                |s, t| s.parse_static_param_decl(t),
            )?;
            toks = t;
            self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
            toks = &toks[1..];
            sp
        } else {
            Vec::new()
        };

        let has_params = self.expect(toks, TokInfo::OpenPar, "(").is_ok();
        let params = if has_params {
            let (t, pars) = self.parse_var_decl(&toks[1..], true)?;
            toks = t;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            toks = &toks[1..];
            pars
        } else {
            Vec::new()
        };

        let returns = if has_params {
            self.expect(toks, TokInfo::Returns, "expected returns")?;
            self.expect(&toks[1..], TokInfo::OpenPar, "expected (")?; // TODO: check if parenthesis are always required after returns
            let (t, rets) = self.parse_var_decl(&toks[2..], true)?;
            toks = t;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            toks = &toks[1..];
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
            let (toks, vars) = if self.expect(toks, TokInfo::Var, "var").is_ok() {
                self.parse_var_decl(&toks[1..], false)?
            } else {
                (toks, vec![])
            };
            // TODO: local constants

            self.expect(toks, TokInfo::Let, "expected let")?;
            let (toks, body) = self.parse_node_body(&toks[1..])?;
            self.expect(toks, TokInfo::Tel, "expected tel")?;
            let toks = if self.expect(&toks[1..], TokInfo::Semicolon, ";").is_ok() {
                &toks[2..]
            } else {
                &toks[1..]
            };

            Ok((
                toks,
                vec![Spanned::fusion(
                    start_span,
                    toks[0].span.clone(),
                    Decl::Node {
                        name,
                        params,
                        outputs: returns,
                        is_unsafe,
                        is_function: !is_node,
                        static_params,
                        vars,
                        body,
                    },
                )],
            ))
        }
    }

    fn parse_static_param(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, StaticParam<'a, 'f>>>> {
        // TODO
        Ok((toks, vec![]))
    }

    fn parse_var_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        optional_final_semicolon: bool,
    ) -> Res<'a, 'f, Vec<Spanned<'f, VariableDecl<'a, 'f>>>> {
        let (toks, decls) = self.parse_many(
            toks,
            Some(TokInfo::Semicolon),
            |_, t| match t.get(0).map(|t| t.item.clone()) {
                Some(
                    TokInfo::Semicolon
                    | TokInfo::Ident(_)
                    | TokInfo::Coma
                    | TokInfo::Colon
                    | TokInfo::Hat,
                ) => false,
                _ => true,
            },
            |s, t| {
                let (t, names) = s.parse_many(
                    t,
                    Some(TokInfo::Coma),
                    |_, t| match t.get(0).map(|t| t.item.clone()) {
                        Some(TokInfo::Ident(_) | TokInfo::Coma) => false,
                        _ => true,
                    },
                    |s, t| {
                        let (t, var_name) = s.parse_id(t)?;
                        Ok((t, vec![var_name]))
                    },
                )?;

                s.expect(t, TokInfo::Colon, "expected :")?;
                let t = &t[1..];
                let (t, ty) = s.parse_ty(t)?;

                Ok((
                    t,
                    names
                        .into_iter()
                        .map(|name| Spanned {
                            span: name.span.clone(),
                            item: VariableDecl {
                                name,
                                ty: ty.clone(),
                            },
                        })
                        .collect(),
                ))
            },
        )?;
        let final_semicolon = self.expect(toks, TokInfo::Semicolon, "expected ;");
        if !optional_final_semicolon && final_semicolon.is_err() {
            return Err(final_semicolon.unwrap_err());
        } else if final_semicolon.is_err() {
            Ok((toks, decls))
        } else {
            Ok((&toks[1..], decls))
        }
    }

    fn parse_ty(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Ty<'a, 'f>> {
        // TODO: handle named types
        let start = toks[0].span.clone();
        let base = if self.expect(toks, TokInfo::Int, "int").is_ok() {
            BaseTy::Int
        } else if self.expect(toks, TokInfo::Real, "real").is_ok() {
            BaseTy::Real
        } else if self.expect(toks, TokInfo::Bool, "bool").is_ok() {
            BaseTy::Bool
        } else {
            return Err(Error::UnexpectedToken(toks, "expected int, bool or real"));
        };
        let (toks, len, end) = if self.expect(&toks[1..], TokInfo::Hat, "^").is_ok() {
            let (t, l) = self.parse_expr(&toks[2..])?;
            let end = l.span.clone();
            (t, Some(l), end)
        } else {
            (&toks[1..], None, start.clone())
        };

        Ok((toks, Spanned::fusion(start, end, Ty { base, len })))
    }

    fn parse_static_param_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>> {
        self.parse_many(
            toks,
            Some(TokInfo::Semicolon),
            |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
            |s, t| {
                let start_span = t[0].span.clone();
                if s.expect(t, TokInfo::Type, "type").is_ok() {
                    let (t, name) = s.parse_id(&t[1..])?;
                    Ok((
                        t,
                        vec![Spanned::fusion(
                            start_span,
                            name.span.clone(),
                            StaticParamDecl::Ty { name },
                        )],
                    ))
                } else if s.expect(t, TokInfo::Const, "const").is_ok() {
                    let (t, name) = s.parse_id(&t[1..])?;
                    s.expect(t, TokInfo::Colon, "expected :")?;
                    let (t, ty) = s.parse_ty(&t[1..])?;
                    Ok((
                        t,
                        vec![Spanned::fusion(
                            start_span,
                            ty.span.clone(),
                            StaticParamDecl::Const { name, ty },
                        )],
                    ))
                } else {
                    let is_unsafe = s.expect(t, TokInfo::Unsafe, "unsafe").is_ok();
                    let t = if is_unsafe { &t[1..] } else { t };
                    let is_node = s.expect(t, TokInfo::Node, "node").is_ok();
                    if !is_node {
                        s.expect(t, TokInfo::Function, "expected function or node")?;
                    }
                    let t = &t[1..];
                    let (t, name) = s.parse_id(t)?;
                    s.expect(t, TokInfo::OpenPar, "expected (")?;
                    let (t, params) = s.parse_var_decl(&t[1..], true)?;
                    s.expect(t, TokInfo::ClosePar, "expected )")?;
                    s.expect(&t[1..], TokInfo::Returns, "expected returns")?;
                    s.expect(&t[2..], TokInfo::OpenPar, "expected (")?;
                    let (t, outputs) = s.parse_var_decl(&t[3..], true)?;
                    s.expect(t, TokInfo::ClosePar, "expected )")?;
                    Ok((
                        &t[1..],
                        vec![Spanned::fusion(
                            start_span,
                            t[0].span.clone(),
                            StaticParamDecl::Node {
                                is_function: !is_node,
                                is_unsafe,
                                params,
                                outputs,
                                name,
                            },
                        )],
                    ))
                }
            },
        )
    }

    fn parse_node_body(
        &mut self,
        toks: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, BodyItem<'a, 'f>>>> {
        self.parse_many(
            toks,
            Some(TokInfo::Semicolon),
            |s, t| s.expect(t, TokInfo::Tel, "tel").is_ok(),
            |s, t| {
                let start = t[0].span.clone();
                if s.expect(t, TokInfo::Assert, "assert").is_ok() {
                    let expr = s.parse_expr(&t[1..])?;
                    Ok((
                        t,
                        vec![Spanned::fusion(
                            start,
                            t[0].span.clone(),
                            BodyItem::Assert(expr),
                        )],
                    ))
                } else {
                    let (t, left) = s.parse_left_items(t)?;
                    s.expect(t, TokInfo::Equal, "expected =")?;
                    let expr = s.parse_expr(&t[1..])?;
                    Ok((
                        t,
                        vec![Spanned::fusion(
                            start,
                            t[0].span.clone(),
                            BodyItem::Equation(left, expr),
                        )],
                    ))
                }
            },
        )
    }

    fn parse_left_item(
        &mut self,
        t: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> SpannedRes<'a, 'f, LeftItem<'a, 'f>> {
        // TODO
        let (t, name) = self.parse_id(t)?;
        Ok((
            t,
            Spanned {
                span: name.span.clone(),
                item: LeftItem::Ident(name),
            },
        ))
    }

    fn parse_left_items(
        &mut self,
        t: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, LeftItem<'a, 'f>>>> {
        self.parse_many(
            t,
            Some(TokInfo::Coma),
            |s, t| s.expect(t, TokInfo::Equal, "=").is_ok(),
            |s, t| {
                let (t, item) = s.parse_left_item(t)?;
                Ok((t, vec![item]))
            },
        )
    }
}
