use crate::lexer::{Tok, TokInfo};
use crate::location::{Spanned};

pub struct Parser;

#[derive(Debug)]
pub enum Error<'a, 'f> {
    UnexpectedToken(&'a [Tok<'a, 'f>])
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

#[derive(Clone)]
#[derive(Debug)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Expr {
    Const(ConstValue),
}

type Res<'a, 'f, T> = Result<(&'a [Tok<'a, 'f>], T), Error<'a, 'f>>;
type SpannedRes<'a, 'f, T> = Res<'a, 'f, Spanned<'f, T>>;

impl Parser {
    fn expect<'a, 'f>(toks: &'a [Tok<'a, 'f>], t: TokInfo<'a>) -> Result<&'a Tok<'a, 'f>, Error<'a, 'f>> {
        match toks.get(0) {
            Some(x) if x.item == t => Ok(x),
            _ => Err(Error::UnexpectedToken(toks)),
        }
    }

    pub fn parse<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> Result<Ast<'a, 'f>, Error<'a, 'f>> {
        let mut includes = vec![];
        let mut toks = toks;
        while let Ok((t, inc)) = Self::parse_include(toks) {
            includes.push(inc);
            toks = t;
        }

        let decls = if let Ok((t, decls)) = Self::parse_package_body(toks) {
            toks = t;
            decls
        } else if let Ok((t, decls)) = Self::parse_package_list(toks) {
            toks = t;
            decls
        } else {
            return Err(Error::UnexpectedToken(toks));
        };

        Self::expect(toks, TokInfo::EOF)?;

        Ok(Ast { includes, decls })
    }

    fn parse_include<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, &'a str> {
        if let Ok(kw) = Self::expect(toks, TokInfo::Include) {
            match toks.get(1) {
                Some(tok @ Spanned { item: TokInfo::Str(inc), .. }) => Ok((
                    &toks[2..],
                    Spanned::fusion(kw, tok, inc),
                )),
                _ => Err(Error::UnexpectedToken(&toks[1..])),
            }
        } else {
            Err(Error::UnexpectedToken(toks))
        }
    }

    fn parse_package_list<'a, 'f>(_toks: &'a [Tok<'a, 'f>]) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
        todo!()
    }
    
    fn parse_package_body<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
        let mut decls = Vec::new();
        let mut toks = toks;
        while let Ok((t, mut decl)) = Self::parse_const_decl(toks) {
            decls.append(&mut decl);
            toks = t;
        }
        Ok((toks, decls))
    }

    fn parse_const_decl<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> Res<'a, 'f, Vec<Spanned<'f, Decl<'a>>>> {
        let kw = Self::expect(toks, TokInfo::Const)?;
        let mut toks = toks;
        let (t, id) = Self::parse_id(&toks[1..])?;
        toks = t;
        let mut ids = vec![id];
        while Self::expect(toks, TokInfo::Coma).is_ok() {
            let (t, id) = Self::parse_id(&toks[1..])?;
            ids.push(id);
            toks = t;
        }

        let ty = if Self::expect(toks, TokInfo::Colon).is_ok() {
            let (t, id) = Self::parse_id(&toks[1..])?;
            toks = t;
            Some(id)
        } else {
            None
        };

        let expr = if ids.len() == 1 && Self::expect(toks, TokInfo::Equal).is_ok() {
            let (t, expr) = Self::parse_expr(&toks[1..])?;
            toks = t;
            Some(expr)
        } else {
            None
        };

        Self::expect(toks, TokInfo::Semicolon)?;

        let ty = ty.map(|x| x.item);
        let value = expr.map(|x| x.item);
        Ok((&toks[1..], ids.into_iter().map(|i| Spanned::fusion(kw, &i, Decl::Const {
            name: i.item,
            ty,
            value: value.clone(),
        })).collect()))
    }

    fn parse_id<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, &'a str> {
        match toks.get(0) {
            Some(tok @ Spanned { item: TokInfo::Ident(x), .. }) => Ok((&toks[1..], Spanned { span: tok.span.clone(), item: x })),
            _ => Err(Error::UnexpectedToken(&toks)),
        }
    }

    fn parse_expr<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr> {
        let cst = Self::parse_const(toks);
        cst
    }

    fn parse_const<'a, 'f>(toks: &'a [Tok<'a, 'f>]) -> SpannedRes<'a, 'f, Expr> {
        match toks.get(0) {
            Some(tok @ Spanned { item: TokInfo::IConst(i), .. }) => Ok((&toks[1..], Spanned { span: tok.span.clone(), item: Expr::Const(ConstValue::Int(*i)) })),
            _ => Err(Error::UnexpectedToken(&toks)),
        }
    }
}
