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
        effective_node: Spanned<'f, (Spanned<'f, &'a str>, Vec<Spanned<'f, Expr<'a, 'f>>>)>,
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
    Bool(bool),
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
    StructAccess(Spanned<'f, Box<Expr<'a, 'f>>>, &'a str),
    NamedClock(Spanned<'f, &'a str>, Box<Option<Spanned<'f, &'a str>>>),
    CallByName(
        Spanned<'f, &'a str>,
        Vec<Spanned<'f, Expr<'a, 'f>>>,
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
    Sharp,
    FieldAccess,
    Hat,
    Nor,
}

#[derive(Debug, Clone)]
pub enum TernaryOp {
    IfThenElse,
    WithThenElse,
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
                None => self.$next(toks),
                Some(op_index) => {
                    let (t, lhs) = self.$name(&toks[..op_index])?;
                    let (t, rhs) = self.$next(&t[1..])?;
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
                    self.expect(
                        toks,
                        s.clone(),
                        Box::leak(format!("expected separator {:?}", s,).into_boxed_str()),
                    )?;
                    toks = &toks[1..];

                    if end(self, toks) {
                        break;
                    }
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
    parse_bin!(LEFT parse_xor, Xor, "xor", parse_and);
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
    parse_bin!(NONE parse_sharp, Sharp, "#", parse_nor);
    parse_bin!(NONE parse_nor, Nor, "nor", parse_hat);
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
        if self.expect(toks, TokInfo::OpenBracket, "[").is_ok() {
            todo!()
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
                    Some(TokInfo::Coma),
                    |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                    |s, t| {
                        let (toks, expr) = reportable!(s, parse_expr, t);
                        Ok((toks, vec![expr]))
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
                    Some(TokInfo::Coma),
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
        Ok(reportable!(self, parse_if, toks))
        // TODO
        // - #(EXPR , EXPR) // TODO: the current impl has the syntax EXPR # EXPR
        // - nor (EXPR , EXPR) // TODO: the current implementation has the syntax EXPR nor EXPR
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
            let effective_static_params = if self.expect(toks, TokInfo::OpenStaticPar, "<<").is_ok()
            {
                // TODO: the separator may be a ; too
                let (t, params) = self.parse_many(
                    &toks[1..],
                    Some(TokInfo::Coma),
                    |s, t| s.expect(t, TokInfo::CloseStaticPar, ">>").is_ok(),
                    |s, t| {
                        let (t, expr) = reportable!(s, parse_expr, t);
                        Ok((t, vec![expr]))
                    },
                )?;
                toks = t;
                self.expect(toks, TokInfo::CloseStaticPar, "expected >>")?;
                toks = &toks[1..];
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
            toks = if self.expect(toks, TokInfo::Semicolon, ";").is_ok() {
                &toks[1..]
            } else {
                toks
            };

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
                    let (t, expr) = s.parse_expr(&t[1..])?;
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
        t: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> SpannedRes<'a, 'f, LeftItem<'a, 'f>> {
        // TODO
        let (t, name) = self.parse_id(t)?;
        Ok((t, name.clone().map(|_| LeftItem::Ident(name))))
    }

    fn parse_left_items(
        &mut self,
        t: &'a [Spanned<'f, TokInfo<'a>>],
    ) -> Res<'a, 'f, Vec<Spanned<'f, LeftItem<'a, 'f>>>> {
        self.parse_many(
            t,
            Some(TokInfo::Coma),
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
