use crate::lexer::{Tok, TokInfo};
use crate::location::Spanned;

pub struct Parser<'a, 'f> {
    pub errors: Vec<Error<'a, 'f>>,
}

#[derive(Debug)]
pub enum Error<'a, 'f> {
    UnexpectedToken(&'a [Tok<'a, 'f>], &'static str),
    ReportedError,
}

#[derive(Debug)]
pub struct Ast<'a, 'f> {
    includes: Vec<Spanned<'f, &'a str>>,
    decls: Vec<Spanned<'f, Decl<'a, 'f>>>,
}

#[derive(Debug)]
pub enum Decl<'a, 'f> {
    Const {
        name: Ident<'a, 'f>,
        /// May be None for external constants
        value: Option<Spanned<'f, Expr<'a, 'f>>>,
        ty: Option<Ty<'a, 'f>>,
    },
    Ty {
        name: Spanned<'f, Ident<'a, 'f>>,
        value: Spanned<'f, Ty<'a, 'f>>,
    },
    ExternalNode {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, Ident<'a, 'f>>,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
    },
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, Ident<'a, 'f>>,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        vars: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        body: Vec<Spanned<'f, BodyItem<'a, 'f>>>,
    },
    AliasNode {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, Ident<'a, 'f>>,
        static_params: Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        effective_node: Spanned<'f, (Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>)>,
    },
}

#[derive(Debug, Clone)]
pub enum StaticArg<'a, 'f> {
    Expr(Spanned<'f, Expr<'a, 'f>>),
    Predefined(Spanned<'f, PredefinedItem>),
    Ty(Spanned<'f, Ty<'a, 'f>>),
    Node(Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>),
}

#[derive(Debug, Clone)]
pub enum PredefinedItem {
    Unary(UnaryOp),
    Binary(BinaryOp),
    NAry(NAryOp),
    Map,
    Fold,
    Red,
}

#[derive(Debug)]
pub enum StaticParamDecl<'a, 'f> {
    Const {
        name: Spanned<'f, Ident<'a, 'f>>,
        ty: Spanned<'f, Ty<'a, 'f>>,
    },
    Ty {
        name: Spanned<'f, Ident<'a, 'f>>,
    },
    Node {
        is_unsafe: bool,
        is_function: bool,
        name: Spanned<'f, Ident<'a, 'f>>,
        params: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
        outputs: Vec<Spanned<'f, VariableDecl<'a, 'f>>>,
    },
}
#[derive(Clone, Debug)]
pub struct Variable<'a, 'f> {
    _name: &'a str,
    _name2: &'f str,
}
#[derive(Clone, Debug)]
pub struct VariableDecl<'a, 'f> {
    name: Spanned<'f, Ident<'a, 'f>>,
    ty: Spanned<'f, Ty<'a, 'f>>,
}

#[derive(Clone, Debug)]
pub struct Ty<'a, 'f> {
    len: Option<Spanned<'f, Expr<'a, 'f>>>,
    base: BaseTy<'a, 'f>,
}

#[derive(Clone, Debug)]
pub enum BaseTy<'a, 'f> {
    Bool,
    Int,
    Real,
    Named(Ident<'a, 'f>),
}

#[derive(Clone, Debug)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum Expr<'a, 'f> {
    Const(ConstValue),
    Ident(Ident<'a, 'f>),
    Unary(Spanned<'f, UnaryOp>, Spanned<'f, Box<Expr<'a, 'f>>>),
    Binary(
        Spanned<'f, BinaryOp>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
    ),
    NAry(NAryOp, Spanned<'f, Vec<Spanned<'f, Expr<'a, 'f>>>>),
    Slice(
        Spanned<'f, Box<Expr<'a, 'f>>>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
        Spanned<'f, Box<Expr<'a, 'f>>>,
        Option<Spanned<'f, Box<Expr<'a, 'f>>>>,
    ),
    Ternary {
        op: TernaryOp,
        condition: Spanned<'f, Box<Expr<'a, 'f>>>,
        then: Spanned<'f, Box<Expr<'a, 'f>>>,
        otherwise: Spanned<'f, Box<Expr<'a, 'f>>>,
    },
    StructAccess(Spanned<'f, Box<Expr<'a, 'f>>>, Ident<'a, 'f>),
    NamedClock(
        Spanned<'f, Ident<'a, 'f>>,
        Box<Option<Spanned<'f, Ident<'a, 'f>>>>,
    ),
    CallByName(
        Spanned<'f, Ident<'a, 'f>>,
        Vec<Spanned<'f, StaticArg<'a, 'f>>>,
        Vec<Spanned<'f, Expr<'a, 'f>>>,
    ),
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
    Default,
    And,
    Or,
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
    Power,
    Concat,
    Index,
    Range,
    Div,
    Prod,
    Step,
    // TODO: maybe this could be merged with Mod?
    // same for Div/Slash btw
    Percent,
    FieldAccess,
    Hat,
}

#[derive(Debug, Clone)]
pub enum TernaryOp {
    IfThenElse,
    WithThenElse,
}

#[derive(Debug, Clone)]
pub enum NAryOp {
    Xor,
    Nor,
    Array,
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
    Ident(Spanned<'f, Ident<'a, 'f>>),
    Tuple(Spanned<'f, Vec<LeftItem<'a, 'f>>>),
    Field(
        Box<Spanned<'f, LeftItem<'a, 'f>>>,
        Spanned<'f, Ident<'a, 'f>>,
    ),
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

#[derive(Clone, Debug)]
pub enum Ident<'a, 'f> {
    Short {
        id: Spanned<'f, &'a str>,
        pragma: Option<(&'a str, &'a str)>,
    },
    Long(Spanned<'f, &'a str>, Box<Ident<'a, 'f>>),
}

type Res<'a, 'f, T> = Result<(&'a [Tok<'a, 'f>], T), Error<'a, 'f>>;
type SpannedRes<'a, 'f, T> = Res<'a, 'f, Spanned<'f, T>>;

macro_rules! parse_bin {
    (LEFT $name:ident, $op:ident, $op2:ident, $disp:expr, $next:ident) => {
        fn $name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
            let start = toks[0].span.clone();
            let (toks, lhs) = self.$next(toks)?;
            if let Ok(op) = self.expect(toks, TokInfo::$op, concat!("expected ", $disp)) {
                let (toks, rhs) = self.$name(&toks[1..])?;
                Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        rhs.span.clone(),
                        Expr::Binary(
                            op.map_ref(|_| BinaryOp::$op2),
                            lhs.boxed(),
                            rhs.boxed(),
                        ),
                    ),
                ))
            } else {
                Ok((toks, lhs))
            }
        }
    };
    (NONE $name:ident, $op:ident, $op2:ident, $disp:expr, $next:ident) => {
        fn $name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
            let start = toks[0].span.clone();
            let (toks, lhs) = self.$next(toks)?;
            if let Ok(op) = self.expect(toks, TokInfo::$op, concat!("expected ", $disp)) {
                let (toks, rhs) = self.$next(&toks[1..])?;
                Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        rhs.span.clone(),
                        Expr::Binary(
                            op.map_ref(|_| BinaryOp::$op2),
                            lhs.boxed(),
                            rhs.boxed(),
                        ),
                    ),
                ))
            } else {
                Ok((toks, lhs))
            }
        }
    };
    (RIGHT $name:ident, $op:ident, $op2:ident, $disp:expr, $next:ident) => {
        fn $name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
            let start = toks[0].span.clone();
            let op_index = toks.iter().position(|t| t.item == TokInfo::$op);
            match op_index {
                None | Some(0) => self.$next(toks),
                Some(op_index) => {
                    if let Ok((left_toks, lhs)) = self.$name(&toks[..op_index]) {
                        if !left_toks.is_empty() { // parsing the LHS actually failed
                            return self.$next(toks);
                        }
                        let (t, rhs) = self.$next(&toks[op_index + 1..])?;
                        Ok((
                            t,
                            Spanned::fusion(
                                start,
                                rhs.span.clone(),
                                Expr::Binary(
                                    toks[op_index].map_ref(|_| BinaryOp::$op2),
                                    lhs.boxed(),
                                    rhs.boxed(),
                                ),
                            ),
                        ))
                    } else {
                        self.$next(toks)
                    }
                }
            }
        }
    };
    ($kind:tt $name:ident, $op:ident, $disp:expr, $next:ident) => {
        parse_bin!($kind $name, $op, $op, $disp, $next);
    };
}

macro_rules! parse_un {
    ($name:ident, $op:ident, $op2:ident, $disp:expr, $next:ident) => {
        fn $name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
            let start = toks[0].span.clone();
            if let Ok(op) = self.expect(toks, TokInfo::$op, concat!("expected ", $disp)) {
                let (toks, expr) = self.parse_expr(&toks[1..])?;
                Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        expr.span.clone(),
                        Expr::Unary(op.map_ref(|_| UnaryOp::$op2), expr.boxed()),
                    ),
                ))
            } else {
                self.$next(toks)
            }
        }
    };
    ($name:ident, $op:ident, $disp:expr, $next:ident) => {
        parse_un!($name, $op, $op, $disp, $next);
    };
}

macro_rules! parse_tern {
    ($name:ident, $op1:ident, $op2:ident, $op3:ident, $parser_op:ident, $disp1:expr, $disp2:expr, $disp3:expr, $next:ident) => {
        fn $name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
            let start = toks[0].span.clone();
            if let Ok(_) = self.expect(toks, TokInfo::$op1, concat!("expected ", $disp1)) {
                let cond_res = self.parse_expr(&toks[1..]);
                let (toks, cond) = self.report_now(cond_res)?;
                self.expect(toks, TokInfo::$op2, concat!("expected ", $disp2))?;
                let (toks, then) = self.parse_expr(&toks[1..])?;
                self.expect(toks, TokInfo::$op3, concat!("expected ", $disp3))?;
                let (toks, otherwise) = self.parse_expr(&toks[1..])?;
                Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        otherwise.span.clone(),
                        Expr::Ternary {
                            op: TernaryOp::$parser_op,
                            condition: cond.boxed(),
                            then: then.boxed(),
                            otherwise: otherwise.boxed(),
                        },
                    ),
                ))
            } else {
                self.$next(toks)
            }
        }
    };
}

macro_rules! reportable {
    ($self:expr, $fn:ident, $toks:expr) => {{
        let res = $self.$fn($toks);
        $self.report_now(res)?
    }};
}

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

    fn report_now<T>(&mut self, res: Result<T, Error<'a, 'f>>) -> Result<T, Error<'a, 'f>> {
        match res {
            Err(e) => {
                self.errors.push(e);
                Err(Error::ReportedError)
            }
            ok => ok,
        }
    }

    #[track_caller]
    fn expect(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        t: TokInfo<'a>,
        exp: &'static str,
    ) -> Result<&'a Tok<'a, 'f>, Error<'a, 'f>> {
        match toks.get(0) {
            Some(x) if x.item == t => Ok(x),
            _ => Err(Error::UnexpectedToken(
                toks,
                Box::leak(
                    format!("{} (line {})", exp, std::panic::Location::caller().line())
                        .into_boxed_str(),
                ),
            )),
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
    #[track_caller]
    fn parse_many<F, E, T>(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        sep: &'a [TokInfo<'a>],
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
        'not_done: while toks.len() > 1 && !end(self, toks) {
            if first {
                first = false;
            } else {
                let mut err = None;
                'sep: for s in sep {
                    // dbg!(&toks[..3]);
                    match (err, self.expect(
                        toks,
                        s.clone(),
                        Box::leak(format!("expected separator {:?}", s,).into_boxed_str()),
                    )) {
                        (None, Err(e)) => err = Some(e),
                        (_, Ok(_)) => {
                            err = None;
                            toks = &toks[1..];
                            break 'sep;
                        },
                        (e, _) => err = e,
                    }
                }

                if end(self, toks) {
                    break 'not_done;
                }

                if let Some(err) = err {
                    return Err(err);
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
            &[],
            |_, _| false,
            |s, t| {
                s.parse_const_decl(t)
                    .or_else(|e| choose_err(e, s.parse_node_decl(t)))
                    .or_else(|e| choose_err(e, s.parse_type_decl(t)))
            },
        )
    }

    fn parse_const_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        let kw = self.expect(toks, TokInfo::Const, "expected const keyword")?;
        self.parse_many(
            &toks[1..],
            &[TokInfo::Semicolon],
            |s, t| s.expect(t, TokInfo::Const, "const").is_ok()
                || s.expect(t, TokInfo::Type, "type").is_ok()
                || s.expect(t, TokInfo::Extern, "extern").is_ok()
                || s.expect(t, TokInfo::Unsafe, "unsafe").is_ok()
                || s.expect(t, TokInfo::Node, "node").is_ok()
                || s.expect(t, TokInfo::Function, "function").is_ok(),
            |s, t| {
                // TODO: refactor id parsing with parse_many?
                let (t, id) = s.parse_id(&t)?;
                let mut ids = vec![id];
                let t = {
                    let mut toks = t;
                    while s.expect(toks, TokInfo::Coma, "expected ,").is_ok() {
                        let (t, id) = s.parse_id(&t[1..])?;
                        ids.push(id);
                        toks = t;
                    }
                    toks
                };

                dbg!(&t[..3]);
                let (t, ty) = if s.expect(t, TokInfo::Colon, "expected :").is_ok() {
                    let (t, ty) = s.parse_ty(&t[1..])?;
                    (t, Some(ty))
                } else {
                    (t, None)
                };

                let (t, expr) = if ids.len() == 1 && s.expect(t, TokInfo::Equal, "expected =").is_ok() {
                    let (t, expr) = s.parse_expr(&t[1..])?;
                    (t, Some(expr))
                } else {
                    (t, None)
                };


                let ty = ty.map(|x| x.item);
                Ok((
                    t,
                    ids.into_iter()
                        .map(|i| {
                            Spanned::fusion(
                                kw,
                                &i,
                                Decl::Const {
                                    name: i.item.clone(),
                                    ty: ty.clone(),
                                    value: expr.clone(),
                                },
                            )
                        })
                        .collect(),
                ))
            },
        )
    }

    fn parse_type_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        self.expect(toks, TokInfo::Type, "expected type")?;
        self.parse_many(
            &toks[1..],
            &[TokInfo::Semicolon],
            |s, t| s.expect(t, TokInfo::Const, "const").is_ok()
                || s.expect(t, TokInfo::Type, "type").is_ok()
                || s.expect(t, TokInfo::Extern, "extern").is_ok()
                || s.expect(t, TokInfo::Unsafe, "unsafe").is_ok()
                || s.expect(t, TokInfo::Node, "node").is_ok()
                || s.expect(t, TokInfo::Function, "function").is_ok(),
            |s, t| {
                let start = t[0].span.clone();
                let (t, name) = s.parse_id(t)?;
                s.expect(t, TokInfo::Equal, "expected =")?;
                let (t, ty) = s.parse_ty(&t[1..])?;
                Ok((
                    t,
                    vec![Spanned::fusion(
                        start,
                        t[0].span.clone(),
                        Decl::Ty { name, value: ty },
                    )],
                ))
            }
        )
    }

    #[track_caller]
    fn parse_id(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Ident<'a, 'f>> {
        let id = match toks.get(0) {
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::Ident(x),
                    ..
                },
            ) => Spanned {
                span: tok.span.clone(),
                item: *x,
            },
            // TODO: check exhaustiveness
            // TODO: maybe they are actually accepted only when fully qualified (e.g Lustre::and,
            // not just and)
            Some(Spanned { item: TokInfo::And, span }) => Spanned { span: span.clone(), item: "and" },
            Some(Spanned { item: TokInfo::Or, span }) => Spanned { span: span.clone(), item: "or" },
            Some(Spanned { item: TokInfo::Xor, span }) => Spanned { span: span.clone(), item: "xor" },
            Some(Spanned { item: TokInfo::Nor, span }) => Spanned { span: span.clone(), item: "nor" },
            Some(Spanned { item: TokInfo::Not, span }) => Spanned { span: span.clone(), item: "not" },
            _ => {
                return Err(Error::UnexpectedToken(
                    &toks,
                    Box::leak(
                        format!(
                            "expected an identifier (line {})",
                            std::panic::Location::caller().line()
                        )
                        .into_boxed_str(),
                    ),
                ))
            }
        };
        if self.expect(&toks[1..], TokInfo::DoubleColon, "::").is_ok() {
            let (toks, next) = self.parse_id(&toks[2..])?;
            Ok((
                toks,
                Spanned::fusion(
                    id.span.clone(),
                    next.span.clone(),
                    Ident::Long(id, Box::new(next.item)),
                ),
            ))
        } else {
            Ok((
                &toks[1..],
                id.clone().map(
                    |_| Ident::Short { id, pragma: None }, /* TODO: pragma */
                ),
            ))
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

    parse_tern!(parse_if, If, Then, Else, IfThenElse, "if", "then", "else", parse_with);
    parse_tern!(
        parse_with,
        With,
        Then,
        Else,
        WithThenElse,
        "with",
        "then",
        "else",
        parse_concat
    );
    parse_bin!(LEFT parse_concat, Bar, Concat, "|", parse_default);
    parse_bin!(LEFT parse_default, Arrow, Default, "->", parse_step);
    parse_bin!(NONE parse_step, Step, Step, "step", parse_range);
    parse_bin!(NONE parse_range, CDots, Range, "..", parse_impl);
    parse_bin!(RIGHT parse_impl, Impl, "=>", parse_or);
    parse_bin!(LEFT parse_or, Or, "or", parse_xor);

    fn parse_xor(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = toks[0].span.clone();
        let (toks, lhs) = self.parse_and(toks)?;
        if let Ok(op) = self.expect(toks, TokInfo::Xor, "expected xor") {
            let (toks, rhs) = self.parse_xor(&toks[1..])?;
            match rhs.item {
                // TODO: this will build one n-ary expression from `a xor #(b, c)`, which is maybe
                // not wanted?
                Expr::NAry(NAryOp::Xor, mut items) => {
                    let mut result = Vec::with_capacity(items.item.len() + 1);
                    result.push(lhs);
                    result.append(&mut items.item);
                    Ok((
                        toks,
                        Spanned::fusion(
                            start.clone(),
                            toks[0].span.clone(),
                            Expr::NAry(
                                NAryOp::Xor,
                                Spanned::fusion(start, items.span.clone(), result),
                            ),
                        ),
                    ))
                }
                _ => Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        rhs.span.clone(),
                        Expr::NAry(
                            NAryOp::Xor,
                            Spanned::fusion(lhs.span.clone(), rhs.span.clone(), vec![lhs, rhs]),
                        ),
                    ),
                )),
            }
        } else {
            Ok((toks, lhs))
        }
    }

    parse_bin!(LEFT parse_and, And, "and", parse_lt);
    // TODO: this allow this kind of expr, while it should not
    // a < b <= c >= d
    parse_bin!(NONE parse_lt, Lt, "<", parse_lte);
    parse_bin!(NONE parse_lte, Lte, "<=", parse_gt);
    parse_bin!(NONE parse_gt, Gt, ">", parse_gte);
    parse_bin!(NONE parse_gte, Gte, ">=", parse_eq);
    parse_bin!(NONE parse_eq, Equal, "=", parse_neq);
    parse_bin!(NONE parse_neq, Neq, "!=", parse_not);
    parse_un!(parse_not, Not, "not", parse_add);
    parse_bin!(LEFT parse_add, Plus, "+", parse_minus);
    parse_bin!(LEFT parse_minus, Minus, "-", parse_mul);
    parse_bin!(LEFT parse_mul, Star, Prod, "*", parse_slash);
    parse_bin!(LEFT parse_slash, Slash, "/", parse_percent);
    parse_bin!(LEFT parse_percent, Percent, "/", parse_mod);
    parse_bin!(LEFT parse_mod, Mod, Mod, "mod", parse_div);
    parse_bin!(LEFT parse_div, Div, "div", parse_power);
    parse_bin!(LEFT parse_power, Power, "**", parse_when);
    parse_bin!(LEFT parse_when, When, "when", parse_real_to_int); // TODO: rhs can only be a clock expression
    parse_un!(parse_real_to_int, Int, RealToInt, "int", parse_int_to_real);
    parse_un!(
        parse_int_to_real,
        Real,
        IntToReal,
        "real",
        parse_minus_unary
    );
    parse_un!(parse_minus_unary, Minus, "-", parse_pre);
    parse_un!(parse_pre, Pre, "pre", parse_current);
    parse_un!(parse_current, Current, "current", parse_sharp);

    fn parse_sharp(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = &toks[0].span;
        if self.expect(toks, TokInfo::Sharp, "#").is_ok() {
            self.expect(&toks[1..], TokInfo::OpenPar, "expected (")?;
            let (toks, items) = self.parse_tuple(&toks[2..])?;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            Ok((
                &toks[1..],
                Spanned::fusion(
                    start.clone(),
                    toks[0].span.clone(),
                    Expr::NAry(NAryOp::Xor, items),
                ),
            ))
        } else {
            self.parse_nor(toks)
        }
    }

    fn parse_nor(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = &toks[0].span;
        if self.expect(toks, TokInfo::Nor, "nor").is_ok() {
            self.expect(&toks[1..], TokInfo::OpenPar, "expected (")?;
            let (toks, items) = self.parse_tuple(&toks[2..])?;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            Ok((
                &toks[1..],
                Spanned::fusion(
                    start.clone(),
                    toks[0].span.clone(),
                    Expr::NAry(NAryOp::Nor, items),
                ),
            ))
        } else {
            self.parse_hat(toks)
        }
    }

    parse_bin!(LEFT parse_hat, Hat, "^", parse_dot);
    parse_bin!(LEFT parse_dot, Dot, FieldAccess, ".", parse_index_or_slice);

    fn parse_index_or_slice(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = toks[0].span.clone();
        let (toks, lhs) = self.parse_array_expr(toks)?;
        if let Ok(bracket) = self.expect(toks, TokInfo::OpenBracket, "[") {
            if let Ok((
                toks,
                Spanned {
                    item: Expr::Binary(_, from, to),
                    ..
                },
            )) = self.parse_range(&toks[1..])
            {
                let (toks, step) = if self.expect(toks, TokInfo::Step, "step").is_ok() {
                    let (t, s) = self.parse_expr(&toks[1..])?;
                    (t, Some(s))
                } else {
                    (toks, None)
                };
                Ok((
                    toks,
                    Spanned::fusion(
                        start,
                        toks[0].span.clone(),
                        Expr::Slice(lhs.boxed(), from, to, step.map(Spanned::boxed)),
                    ),
                ))
            } else {
                let (toks, rhs) = self.parse_expr(&toks[1..])?;
                self.expect(toks, TokInfo::CloseBracket, "expected ]")?;
                Ok((
                    &toks[1..],
                    Spanned::fusion(
                        start,
                        rhs.span.clone(),
                        Expr::Binary(
                            bracket.map_ref(|_| BinaryOp::Index),
                            lhs.boxed(),
                            rhs.boxed(),
                        ),
                    ),
                ))
            }
        } else {
            Ok((toks, lhs))
        }
    }

    fn parse_array_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let start = &toks[0].span;
        if self.expect(toks, TokInfo::OpenBracket, "[").is_ok() {
            let (toks, items) = self.parse_tuple(&toks[1..])?;
            self.expect(toks, TokInfo::CloseBracket, "expected ]")?;
            Ok((&toks[1..], Spanned::fusion(start.clone(), toks[0].span.clone(), Expr::NAry(NAryOp::Array, items))))
        } else {
            self.parse_struct_expr(toks)
        }
    }

    fn parse_struct_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        if self.expect(toks, TokInfo::OpenBrace, "{").is_ok() {
            todo!()
        } else {
            self.parse_paren_expr(toks)
        }
    }

    fn parse_paren_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        if self.expect(toks, TokInfo::OpenPar, "(").is_ok() {
            let (toks, expr) = self.parse_expr(&toks[1..])?;
            self.expect(toks, TokInfo::ClosePar, "expected )")?;
            Ok((&toks[1..], expr))
        } else {
            self.parse_fby(toks)
        }
    }

    parse_bin!(RIGHT parse_fby, FBy, "fby", parse_merge);

    fn parse_merge(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        if self.expect(toks, TokInfo::OpenPar, "(").is_ok() {
            todo!()
        } else {
            self.parse_call_by_pos(toks)
        }
    }

    fn parse_call_by_pos(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        if self.expect(toks, TokInfo::OpenPar, "(").is_ok() {
            todo!()
        } else {
            self.parse_call_by_name(toks)
        }
    }

    fn parse_call_by_name(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let original_toks = toks;
        if let Ok((toks, name)) = self.parse_id(toks) {
            let (toks, static_params) = if self.expect(toks, TokInfo::OpenStaticPar, "<<").is_ok() {
                let (toks, params) = self.parse_many(
                    &toks[1..],
                    &[TokInfo::Coma, TokInfo::Semicolon],
                    |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                    |s, t| {
                        let (toks, expr) = reportable!(s, parse_static_arg, t);
                        Ok((toks, vec![Spanned::fusion(t[0].span.clone(), toks[0].span.clone(), expr)]))
                    },
                )?;
                self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
                (&toks[1..], params)
            } else {
                (toks, Vec::new())
            };

            if self.expect(toks, TokInfo::OpenPar, "expected (").is_ok()
                || !static_params.is_empty()
            {
                let (toks, params) = self.parse_many(
                    &toks[1..],
                    &[TokInfo::Coma],
                    |s, t| s.expect(t, TokInfo::ClosePar, ")").is_ok(),
                    |s, t| {
                        let (toks, expr) = s.parse_expr(t)?;
                        Ok((toks, vec![expr]))
                    },
                )?;
                self.expect(toks, TokInfo::ClosePar, "expected )")?;

                Ok((
                    &toks[1..],
                    Spanned::fusion(
                        name.span.clone(),
                        toks[0].span.clone(),
                        Expr::CallByName(name, static_params, params),
                    ),
                ))
            } else {
                self.parse_expr_term(original_toks)
            }
        } else {
            self.parse_expr_term(toks)
        }
    }

    fn parse_expr_term(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        self.parse_const(toks).or_else(|_| {
            let (t, id) = self.parse_id(toks)?;
            Ok((t, id.map(Expr::Ident)))
        })
    }

    fn parse_expr(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        self.parse_if(toks)
    }

    fn parse_const(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr<'a, 'f>> {
        let neg = self.expect(toks, TokInfo::Minus, "-").is_ok();
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
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::False,
                    ..
                },
            ) => Ok((
                &toks[1..],
                tok.map_ref(|_| Expr::Const(ConstValue::Bool(false))),
            )),
            Some(
                tok
                @
                Spanned {
                    item: TokInfo::True,
                    ..
                },
            ) => Ok((
                &toks[1..],
                tok.map_ref(|_| Expr::Const(ConstValue::Bool(true))),
            )),
            _ => Err(Error::UnexpectedToken(&toks, "expected a constant")),
        }
    }

    fn parse_tuple(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> SpannedRes<'a, 'f, Vec<Spanned<'f, Expr<'a, 'f>>>> {
        let (toks, fst) = reportable!(self, parse_expr, toks);
        let start = fst.span.clone();
        if self.expect(toks, TokInfo::Coma, ",").is_ok() {
            let (toks, mut next) = self.parse_tuple(&toks[1..])?;
            let mut result = Vec::with_capacity(next.item.len() + 1);
            result.push(fst);
            result.append(&mut next.item);
            Ok((toks, Spanned::fusion(start, toks[0].span.clone(), result)))
        } else {
            Ok((
                toks,
                Spanned {
                    span: start,
                    item: vec![fst],
                },
            ))
        }
    }

    fn parse_node_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a, 'f>>>> {
        let start_span = toks[0].span.clone();
        let mut toks = toks;
        let is_unsafe = self.expect(toks, TokInfo::Unsafe, "unsafe keyword").is_ok();
        toks = if is_unsafe { &toks[1..] } else { toks };
        let is_extern = self.expect(toks, TokInfo::Extern, "extern keyword").is_ok();
        toks = if is_extern { &toks[1..] } else { toks };
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
                &[TokInfo::Semicolon],
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

        if is_extern {
            self.expect(toks, TokInfo::Semicolon, "expected ;")?;

            return Ok((&toks[1..], vec![
                Spanned::fusion(
                    start_span,
                    toks[0].span.clone(),
                    Decl::ExternalNode { is_function: !is_node, is_unsafe, name, outputs: returns, params, static_params },
                )
            ]))
        }

        let is_alias = self.expect(toks, TokInfo::Equal, "=").is_ok();
        if !is_alias && !has_params {
            return Err(Error::UnexpectedToken(
                toks,
                "expected a node alias (declared with a =) or parameters",
            ));
        }

        if is_alias {
            toks = &toks[1..];
            let (t, effective_node) = self.parse_effective_node(toks)?;
            toks = if self.expect(t, TokInfo::Semicolon, ";").is_ok() {
                &t[1..]
            } else {
                t
            };


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
            toks = if self.expect(toks, TokInfo::Semicolon, ";").is_ok() {
                &toks[1..]
            } else {
                toks
            };

            let (toks, vars) = if self.expect(toks, TokInfo::Var, "var").is_ok() {
                self.parse_var_decl(&toks[1..], true)? // FIXME: the final semicolon is not optional
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

    fn parse_static_arg(&mut self, toks: &'a [Tok<'a, 'f>]) -> Res<'a, 'f, StaticArg<'a, 'f>> {
        // TODO: the kind of static arg can be explicitely specified with const/type/node/function
        self.parse_effective_node(toks)
            .map(|(t, Spanned { item: (name, args), .. })| (t, StaticArg::Node(name, args)))
            .or_else(|e| {
                choose_err(
                    e,
                    self.parse_predefined(toks)
                        .map(|(t, p)| (t, StaticArg::Predefined(p))),
                )
            })
            .or_else(|e| {
                choose_err(e, self.parse_ty(toks).map(|(t, ty)| (t, StaticArg::Ty(ty))))
            })
            .or_else(|e| {
                choose_err(e, self.parse_expr(toks).map(|(t, expr)| (t, StaticArg::Expr(expr))))
            })
    }

    fn parse_effective_node(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, (Spanned<'f, Ident<'a, 'f>>, Vec<StaticArg<'a, 'f>>)> {
        let name_span = toks[0].span.clone();
        let (toks, effective_node_name) = self.parse_id(toks)?;
        let (toks, effective_static_params) = if self.expect(toks, TokInfo::OpenStaticPar, "<<").is_ok()
        {
            let (toks, params) = self.parse_many(
                &toks[1..],
                &[TokInfo::Coma, TokInfo::Semicolon],
                |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                |s, t| {
                    let (t, expr) = reportable!(s, parse_static_arg, t);
                    Ok((t, vec![expr]))
                },
            )?;
            self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
            (&toks[1..], params)
        } else {
            (toks, Vec::new())
        };

        Ok((toks, Spanned::fusion(
            name_span,
            toks[0].span.clone(),
            (effective_node_name, effective_static_params),
        )))
    }

    fn parse_predefined(&mut self, toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, PredefinedItem> {
        use TokInfo::*;
        // TODO: check the exhaustiveness of this list
        let bin_op = match toks[0].item {
            Plus => Some(BinaryOp::Plus),
            Minus => Some(BinaryOp::Minus),
            Star => Some(BinaryOp::Prod),
            Slash => Some(BinaryOp::Slash),
            Div => Some(BinaryOp::Div),
            Mod => Some(BinaryOp::Mod),
            Percent => Some(BinaryOp::Percent),
            Or => Some(BinaryOp::Or),
            And => Some(BinaryOp::And),
            Impl => Some(BinaryOp::Impl),
            Lt => Some(BinaryOp::Lt),
            Lte => Some(BinaryOp::Lte),
            Gt => Some(BinaryOp::Gt),
            Gte => Some(BinaryOp::Gte),
            Equal => Some(BinaryOp::Equal),
            Neq => Some(BinaryOp::Neq),
            _ => None,
        };
        let un_op = match toks[0].item {
            Minus => Some(UnaryOp::Minus),
            Not => Some(UnaryOp::Not),
            _ => None,
        };
        let n_op = match toks[0].item {
            Xor | Sharp => Some(NAryOp::Xor),
            Nor => Some(NAryOp::Nor),
            _ => None,
        };
        let span = toks[0].span.clone();
        let item = if let Some(bin) = bin_op {
            PredefinedItem::Binary(bin)
        } else if let Some(un) = un_op {
            PredefinedItem::Unary(un)
        } else if let Some(nary) = n_op {
            PredefinedItem::NAry(nary)
        } else {
            return Err(Error::UnexpectedToken(
                toks,
                "expected a predefined operator or node",
            ));
        };
        Ok((&toks[1..], Spanned { span, item }))
    }

    fn parse_var_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
        optional_final_semicolon: bool,
    ) -> Res<'a, 'f, Vec<Spanned<'f, VariableDecl<'a, 'f>>>> {
        let (toks, decls) = self.parse_many(
            toks,
            &[TokInfo::Semicolon],
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
                    &[TokInfo::Coma],
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
                        .map(|name| {
                            name.map_ref(|_| VariableDecl {
                                name: name.clone(),
                                ty: ty.clone(),
                            })
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
        let start = toks[0].span.clone();
        let (toks, base) = if self.expect(toks, TokInfo::Int, "int").is_ok() {
            (&toks[1..], BaseTy::Int)
        } else if self.expect(toks, TokInfo::Real, "real").is_ok() {
            (&toks[1..], BaseTy::Real)
        } else if self.expect(toks, TokInfo::Bool, "bool").is_ok() {
            (&toks[1..], BaseTy::Bool)
        } else {
            let (toks, name) = self.parse_id(toks)?;
            (toks, BaseTy::Named(name.item))
        };
        // TODO: handle types like bool^n^m
        let (toks, len, end) = if self.expect(toks, TokInfo::Hat, "^").is_ok() {
            let (t, l) = reportable!(self, parse_expr, &toks[1..]);
            let end = l.span.clone();
            (t, Some(l), end)
        } else {
            (toks, None, toks[0].span.clone())
        };

        Ok((toks, Spanned::fusion(start, end, Ty { base, len })))
    }

    fn parse_static_param_decl(
        &mut self,
        toks: &'a [Tok<'a, 'f>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, StaticParamDecl<'a, 'f>>>> {
        self.parse_many(
            toks,
            &[TokInfo::Semicolon],
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
            &[TokInfo::Semicolon],
            |s, t| s.expect(t, TokInfo::Tel, "tel").is_ok(),
            |s, t| {
                let start = t[0].span.clone();
                if s.expect(t, TokInfo::Assert, "assert").is_ok() {
                    let (t, expr) = reportable!(s, parse_expr, &t[1..]);
                    Ok((
                        t,
                        vec![Spanned::fusion(
                            start,
                            t[0].span.clone(),
                            BodyItem::Assert(expr),
                        )],
                    ))
                } else {
                    let (t, left) = reportable!(s, parse_left_items, t);
                    s.expect(t, TokInfo::Equal, "expected =")?;
                    let (t, expr) = reportable!(s, parse_expr, &t[1..]);
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
        toks: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> SpannedRes<'a, 'f, LeftItem<'a, 'f>> {
        // TODO
        if self.expect(toks, TokInfo::OpenPar, "(").is_ok() {
            // TODO: i think the parenthesis are actually optional
            let (toks, items) = self.parse_many(
                &toks[1..],
                &[TokInfo::Coma],
                |s, t| s.expect(t, TokInfo::ClosePar, ")").is_ok(),
                |s, t| {
                    let (toks, inner) = s.parse_left_item(t)?;
                    Ok((toks, vec![inner]))
                },
            )?;
            let spanned_item = Spanned::fusion(
                items[0].span.clone(),
                toks[0].span.clone(),
                items.into_iter().map(|i| i.item).collect(),
            );
            self.expect(toks, TokInfo::ClosePar, ")")?;
            Ok((
                &toks[1..],
                Spanned {
                    span: spanned_item.span.clone(),
                    item: LeftItem::Tuple(spanned_item),
                },
            ))
        } else {
            let (t, name) = self.parse_id(toks)?;
            Ok((t, name.clone().map(|_| LeftItem::Ident(name))))
        }
    }

    fn parse_left_items(
        &mut self,
        t: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, LeftItem<'a, 'f>>>> {
        self.parse_many(
            t,
            &[TokInfo::Coma],
            |s, t| {
                s.expect(t, TokInfo::Equal, "=").is_ok() || s.expect(t, TokInfo::Tel, "tel").is_ok()
            },
            |s, t| {
                let (t, item) = s.parse_left_item(t)?;
                Ok((t, vec![item]))
            },
        )
    }
}

fn choose_err<'a, 'f, T>(
    err: Error<'a, 'f>,
    res: Result<T, Error<'a, 'f>>,
) -> Result<T, Error<'a, 'f>> {
    match (err, res) {
        (_, Ok(x)) => Ok(x),
        (Error::UnexpectedToken(toks1, msg1), Err(Error::UnexpectedToken(toks2, msg2))) => {
            if toks1.len() <= toks2.len() {
                Err(Error::UnexpectedToken(toks1, msg1))
            } else {
                Err(Error::UnexpectedToken(toks2, msg2))
            }
        }
        (e @ Error::UnexpectedToken(_, _), _) | (_, Err(e @ Error::UnexpectedToken(_, _))) => {
            Err(e)
        }
        (e, _) => Err(e),
    }
}
