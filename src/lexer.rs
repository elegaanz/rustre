use crate::location::{Location, Span, Spanned};

pub type Tok<'a, 'f> = Spanned<'f, TokInfo<'a>>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokInfo<'a> {
    EOF,
    Extern,
    Unsafe,
    And,
    Arrow,
    Assert,
    Bar,
    Bool,
    CDots,
    CloseBrace,
    CloseBracket,
    ClosePar,
    CloseStaticPar,
    Colon,
    Coma,
    Const,
    Current,
    Sharp,
    Div,
    DoubleColon,
    Dot,
    Equal,
    Else,
    Enum,
    False,
    Function,
    Gt,
    Gte,
    Hat,
    IConst(i64),
    Ident(&'a str),
    // LongIdent,
    If,
    Impl,
    Int,
    Let,
    Lt,
    Lte,
    Merge,
    Minus,
    Mod,
    Neq,
    Node,
    Nor,
    Not,
    OpenBrace,
    OpenBracket,
    OpenPar,
    OpenStaticPar,
    Operator,
    Or,
    Percent,
    Plus,
    Power,
    Pre,
    FBy,
    RConst(f64),
    Real,
    Returns,
    Semicolon,
    Slash,
    Star,
    Step,
    Struct,
    Tel,
    Then,
    True,
    Type,
    Var,
    When,
    With,
    Xor,
    Model,
    Package,
    Needs,
    Provides,
    Uses,
    Is,
    Body,
    End,
    Include,
    Str(&'a str),
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Grammar {
    Main,
    Str,
    InlineComment,
    Comment(char),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Error {
    UnclosedStr,
    UnclosedComment,
}

pub struct Lexer<'a, 'f> {
    file: &'f str,
    src: &'a str,
}

#[derive(PartialEq, Debug)]
enum MatchResult<'a> {
    NotYetMatched,
    NoMatches,
    Ambiguous(Vec<TokInfo<'a>>),
    Match(LexMatch<'a>),
    ManyMatches(Vec<TokInfo<'a>>),
}

#[derive(PartialEq, Debug)]
enum LexMatch<'a> {
    Tok(TokInfo<'a>),
    Comment(&'a str),
}

impl<'a, 'f> Lexer<'a, 'f> {
    pub fn new(file: &'f str, src: &'a str) -> Self {
        Lexer { file, src }
    }

    pub fn lex(&mut self) -> Result<Vec<Tok>, Error> {
        let total_len = self.src.len();
        let mut res = Vec::with_capacity(total_len / 4);
        let mut start = 0;
        let mut end = 0;
        let mut grammar = Grammar::Main;
        let mut line = 1;
        let mut col = 0;
        while end < total_len {
            let mut last_span = Span::default();
            let mut last_match = MatchResult::NotYetMatched;
            let mut curr_match = MatchResult::NotYetMatched;
            let mut last_end = end;
            while end <= total_len && curr_match != MatchResult::NoMatches {
                let src_slice = &self.src[start..end];
                match src_slice {
                    "" => {}
                    "--" => grammar = Grammar::InlineComment,
                    "/*" => grammar = Grammar::Comment('/'),
                    "(*" => grammar = Grammar::Comment(')'),
                    "\"" => grammar = Grammar::Str,
                    _ => {
                        last_span = self.span(line, col, start, last_end - start);
                        last_match = curr_match;
                        curr_match = match grammar {
                            Grammar::InlineComment => self.match_inline_comm(src_slice),
                            Grammar::Comment(delim) => self.match_comm(src_slice, delim),
                            Grammar::Str => self.match_str(src_slice),
                            Grammar::Main => self.match_tokens(src_slice),
                        };
                        if curr_match != MatchResult::NotYetMatched {
                            grammar = Grammar::Main;
                        }
                    }
                }
                if curr_match != MatchResult::NoMatches {
                    last_end = end;
                    end = self.next_char(end, 1);
                }
            }

            let added_lines = self.src[last_span.start.pos as usize..last_span.end.pos as usize]
                .chars()
                .filter(|c| *c == '\n')
                .count();
            if added_lines != 0 {
                line += added_lines;
                col = 0;
            }
            let added_cols = self.src[last_span.start.pos as usize..last_span.end.pos as usize]
                .lines()
                .last()
                .unwrap_or_default()
                .len();
            col += added_cols;

            if end == total_len + 1 {
                last_match = curr_match;
            }

            match last_match {
                MatchResult::Match(LexMatch::Tok(m)) => {
                    res.push(Spanned {
                        span: last_span.clone(),
                        item: m,
                    });
                    end = last_end;
                }
                MatchResult::ManyMatches(matches) | MatchResult::Ambiguous(matches) => {
                    for m in matches {
                        res.push(Spanned {
                            span: last_span.clone(),
                            item: m,
                        });
                    }
                    end = last_end;
                }
                _ => {}
            }
            start = last_end;
        }
        if grammar == Grammar::Str {
            Err(Error::UnclosedStr)
        } else if let Grammar::Comment(_) = grammar {
            Err(Error::UnclosedComment)
        } else {
            res.push(Spanned {
                span: self.span(line, col, start, 0),
                item: TokInfo::EOF,
            });
            Ok(res)
        }
    }

    fn next_char(&self, from: usize, add: isize) -> usize {
        Self::next_char_in(self.src, from, add)
    }

    fn next_char_in(src: &str, from: usize, add: isize) -> usize {
        let mut new_pos = (from as isize) + add;
        while !src.is_char_boundary(new_pos as usize) && (new_pos as usize) < src.len() {
            new_pos += add.signum();
        }
        new_pos as usize
    }

    fn match_inline_comm(&self, src_slice: &'a str) -> MatchResult<'a> {
        let len = src_slice.len();
        let start = Self::next_char_in(src_slice, len, -1);
        if &src_slice[start..] == "\n" {
            MatchResult::Match(LexMatch::Comment(&src_slice[2..]))
        } else {
            MatchResult::NotYetMatched
        }
    }

    fn match_comm(&self, src_slice: &'a str, delim: char) -> MatchResult<'a> {
        let end = format!("*{}", delim);
        let len = src_slice.len();
        let start = Self::next_char_in(src_slice, len, -2);
        if &src_slice[start..] == &end {
            MatchResult::Match(LexMatch::Comment(&src_slice[2..start]))
        } else {
            MatchResult::NotYetMatched
        }
    }

    fn match_str(&self, src_slice: &'a str) -> MatchResult<'a> {
        let len = src_slice.len();
        let start = Self::next_char_in(src_slice, len, -1);
        if &src_slice[start..] == "\"" {
            MatchResult::Match(LexMatch::Tok(TokInfo::Str(&src_slice[1..start])))
        } else {
            MatchResult::NotYetMatched
        }
    }

    fn match_tokens(&self, src_slice: &'a str) -> MatchResult<'a> {
        match src_slice {
            " " | "\t" | "\n" | "\r" => MatchResult::NoMatches,
            "," => MatchResult::Match(LexMatch::Tok(TokInfo::Coma)),
            ";" => MatchResult::Match(LexMatch::Tok(TokInfo::Semicolon)),
            "::" => MatchResult::Match(LexMatch::Tok(TokInfo::DoubleColon)),
            ":" => MatchResult::Match(LexMatch::Tok(TokInfo::Colon)),
            "->" => MatchResult::Match(LexMatch::Tok(TokInfo::Arrow)),
            "=>" => MatchResult::Match(LexMatch::Tok(TokInfo::Impl)),
            "<=" => MatchResult::Match(LexMatch::Tok(TokInfo::Lte)),
            "<>" => MatchResult::Match(LexMatch::Tok(TokInfo::Neq)),
            ">=" => MatchResult::Match(LexMatch::Tok(TokInfo::Gte)),
            ".." => MatchResult::Match(LexMatch::Tok(TokInfo::CDots)),
            "**" => MatchResult::Match(LexMatch::Tok(TokInfo::Power)),
            "<<" => MatchResult::Match(LexMatch::Tok(TokInfo::OpenStaticPar)),
            ">>" => MatchResult::Match(LexMatch::Tok(TokInfo::CloseStaticPar)),
            "+" => MatchResult::Match(LexMatch::Tok(TokInfo::Plus)),
            "^" => MatchResult::Match(LexMatch::Tok(TokInfo::Hat)),
            "#" => MatchResult::Match(LexMatch::Tok(TokInfo::Sharp)),
            "-" => MatchResult::Ambiguous(vec![TokInfo::Minus]),
            "/" => MatchResult::Match(LexMatch::Tok(TokInfo::Slash)),
            "%" => MatchResult::Match(LexMatch::Tok(TokInfo::Percent)),
            "*" => MatchResult::Match(LexMatch::Tok(TokInfo::Star)),
            "|" => MatchResult::Match(LexMatch::Tok(TokInfo::Bar)),
            "=" => MatchResult::Match(LexMatch::Tok(TokInfo::Equal)),
            "." => MatchResult::Match(LexMatch::Tok(TokInfo::Dot)),
            "(" => MatchResult::Match(LexMatch::Tok(TokInfo::OpenPar)),
            ")" => MatchResult::Match(LexMatch::Tok(TokInfo::ClosePar)),
            "{" => MatchResult::Match(LexMatch::Tok(TokInfo::OpenBrace)),
            "}" => MatchResult::Match(LexMatch::Tok(TokInfo::CloseBrace)),
            "[" => MatchResult::Match(LexMatch::Tok(TokInfo::OpenBracket)),
            "]" => MatchResult::Match(LexMatch::Tok(TokInfo::CloseBracket)),
            "<" => MatchResult::Match(LexMatch::Tok(TokInfo::Lt)),
            ">" => MatchResult::Match(LexMatch::Tok(TokInfo::Gt)),
            "extern" => MatchResult::Match(LexMatch::Tok(TokInfo::Extern)),
            "unsafe" => MatchResult::Match(LexMatch::Tok(TokInfo::Unsafe)),
            "and" => MatchResult::Match(LexMatch::Tok(TokInfo::And)),
            "assert" => MatchResult::Match(LexMatch::Tok(TokInfo::Assert)),
            "bool" => MatchResult::Match(LexMatch::Tok(TokInfo::Bool)),
            "const" => MatchResult::Match(LexMatch::Tok(TokInfo::Const)),
            "current" => MatchResult::Match(LexMatch::Tok(TokInfo::Current)),
            "div" => MatchResult::Match(LexMatch::Tok(TokInfo::Div)),
            "else" => MatchResult::Match(LexMatch::Tok(TokInfo::Else)),
            "enum" => MatchResult::Match(LexMatch::Tok(TokInfo::Enum)),
            "function" => MatchResult::Match(LexMatch::Tok(TokInfo::Function)),
            "false" => MatchResult::Match(LexMatch::Tok(TokInfo::False)),
            "if" => MatchResult::Match(LexMatch::Tok(TokInfo::If)),
            "int" => MatchResult::Match(LexMatch::Tok(TokInfo::Int)),
            "let" => MatchResult::Match(LexMatch::Tok(TokInfo::Let)),
            "mod" => MatchResult::Match(LexMatch::Tok(TokInfo::Mod)),
            "node" => MatchResult::Match(LexMatch::Tok(TokInfo::Node)),
            "not" => MatchResult::Match(LexMatch::Tok(TokInfo::Not)),
            "operator" => MatchResult::Match(LexMatch::Tok(TokInfo::Operator)),
            "or" => MatchResult::Match(LexMatch::Tok(TokInfo::Or)),
            "nor" => MatchResult::Match(LexMatch::Tok(TokInfo::Nor)),
            "fby" => MatchResult::Match(LexMatch::Tok(TokInfo::FBy)),
            "pre" => MatchResult::Match(LexMatch::Tok(TokInfo::Pre)),
            "real" => MatchResult::Match(LexMatch::Tok(TokInfo::Real)),
            "returns" => MatchResult::Match(LexMatch::Tok(TokInfo::Returns)),
            "step" => MatchResult::Match(LexMatch::Tok(TokInfo::Step)),
            "struct" => MatchResult::Match(LexMatch::Tok(TokInfo::Struct)),
            "tel" => MatchResult::Match(LexMatch::Tok(TokInfo::Tel)),
            "type" => MatchResult::Match(LexMatch::Tok(TokInfo::Type)),
            "then" => MatchResult::Match(LexMatch::Tok(TokInfo::Then)),
            "true" => MatchResult::Match(LexMatch::Tok(TokInfo::True)),
            "var" => MatchResult::Match(LexMatch::Tok(TokInfo::Var)),
            "when" => MatchResult::Match(LexMatch::Tok(TokInfo::When)),
            "with" => MatchResult::Match(LexMatch::Tok(TokInfo::With)),
            "xor" => MatchResult::Match(LexMatch::Tok(TokInfo::Xor)),
            "model" => MatchResult::Match(LexMatch::Tok(TokInfo::Model)),
            "package" => MatchResult::Match(LexMatch::Tok(TokInfo::Package)),
            "needs" => MatchResult::Match(LexMatch::Tok(TokInfo::Needs)),
            "provides" => MatchResult::Match(LexMatch::Tok(TokInfo::Provides)),
            "uses" => MatchResult::Match(LexMatch::Tok(TokInfo::Uses)),
            "is" => MatchResult::Match(LexMatch::Tok(TokInfo::Is)),
            "body" => MatchResult::Match(LexMatch::Tok(TokInfo::Body)),
            "end" => MatchResult::Match(LexMatch::Tok(TokInfo::End)),
            "include" => MatchResult::Match(LexMatch::Tok(TokInfo::Include)),
            "merge" => MatchResult::Match(LexMatch::Tok(TokInfo::Merge)),
            x if x.chars().all(char::is_numeric) => {
                MatchResult::Match(LexMatch::Tok(TokInfo::IConst(x.parse::<i64>().unwrap())))
            }
            x if x.chars().all(|c| {
                c.is_numeric() || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-'
            }) && x.chars().any(char::is_numeric)
                && (!(x.contains('+') || x.contains('-'))
                    || x.contains("e-")
                    || x.contains("e+")
                    || x.contains("E+")
                    || x.contains("E-")) =>
            {
                // Maybe we are parsing something like `1..2`
                if x.ends_with('.') {
                    if x.ends_with("..") {
                        MatchResult::ManyMatches(vec![
                            TokInfo::IConst(x[..x.len() - 2].parse::<i64>().unwrap()),
                            TokInfo::CDots,
                        ])
                    } else {
                        MatchResult::Ambiguous(vec![TokInfo::RConst(x.parse::<f64>().unwrap())])
                    }
                } else if x.contains("..") {
                    MatchResult::NoMatches
                } else if x.starts_with('e') || x.starts_with('E') {
                    MatchResult::Match(LexMatch::Tok(TokInfo::Ident(x)))
                } else if x.ends_with('e') || x.ends_with('E') {
                    MatchResult::NotYetMatched
                } else if x.ends_with('-') {
                    let mut tokens = Vec::with_capacity(2);
                    match self.match_tokens(&x[..x.len() - 1]) {
                        MatchResult::Ambiguous(ref mut t) | MatchResult::ManyMatches(ref mut t) => {
                            tokens.append(t)
                        }
                        MatchResult::Match(LexMatch::Tok(m)) => tokens.push(m),
                        _ => {}
                    }
                    tokens.push(TokInfo::Minus);
                    MatchResult::Ambiguous(tokens)
                } else if x.ends_with('+') {
                    let mut tokens = Vec::with_capacity(2);
                    match self.match_tokens(&x[..x.len() - 1]) {
                        MatchResult::Ambiguous(ref mut t) | MatchResult::ManyMatches(ref mut t) => {
                            tokens.append(t)
                        }
                        MatchResult::Match(LexMatch::Tok(m)) => tokens.push(m),
                        _ => {}
                    }
                    tokens.push(TokInfo::Plus);
                    MatchResult::Ambiguous(tokens)
                } else if x.starts_with('-') {
                    let mut tokens = Vec::with_capacity(2);
                    tokens.push(TokInfo::Minus);
                    match self.match_tokens(&x[1..]) {
                        MatchResult::Ambiguous(ref mut t) | MatchResult::ManyMatches(ref mut t) => {
                            tokens.append(t)
                        }
                        MatchResult::Match(LexMatch::Tok(m)) => tokens.push(m),
                        _ => {}
                    }
                    MatchResult::ManyMatches(tokens)
                } else if x.starts_with('+') {
                    let mut tokens = Vec::with_capacity(2);
                    tokens.push(TokInfo::Plus);
                    match self.match_tokens(&x[1..]) {
                        MatchResult::Ambiguous(ref mut t) | MatchResult::ManyMatches(ref mut t) => {
                            tokens.append(t)
                        }
                        MatchResult::Match(LexMatch::Tok(m)) => tokens.push(m),
                        _ => {}
                    }
                    MatchResult::ManyMatches(tokens)
                } else {
                    MatchResult::Match(LexMatch::Tok(TokInfo::RConst(x.parse::<f64>().unwrap())))
                }
            }
            x if x.chars().all(|c| c.is_alphanumeric() || c == '_') => {
                MatchResult::Match(LexMatch::Tok(TokInfo::Ident(x)))
            }
            // yet another special case for real constants
            // this one is for handling things like: 1->pre x
            x if x.ends_with("->") => {
                let mut tokens = Vec::with_capacity(2);
                match self.match_tokens(&x[..x.len() - 2]) {
                    MatchResult::Ambiguous(ref mut t) | MatchResult::ManyMatches(ref mut t) => {
                        tokens.append(t)
                    }
                    MatchResult::Match(LexMatch::Tok(m)) => tokens.push(m),
                    _ => {}
                }
                tokens.push(TokInfo::Arrow);
                MatchResult::ManyMatches(tokens)
            }
            _ => MatchResult::NoMatches,
        }
    }

    fn span(&self, line: usize, col: usize, pos: usize, len: usize) -> Span {
        Span {
            start: Location {
                line: line as u64,
                col: col as u64,
                pos: pos as u64,
            },
            end: Location {
                line: line as u64, // no token can be on multiple lines as far as I know
                col: (col + len) as u64,
                pos: (pos + len) as u64,
            },
            file: self.file,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_tok_info<'a, 'f>(actual: Vec<Tok<'a, 'f>>, expected: Vec<TokInfo<'a>>) {
        dbg!(&actual);
        assert_eq!(actual.len(), expected.len());

        for (ref a, ref e) in actual.iter().zip(expected.iter()) {
            assert_eq!(a.item, **e);
        }
    }

    fn test_lexer<'a>(src: &'a str, expected: Vec<TokInfo<'a>>) {
        let mut lex = Lexer::new("main.lus", src);
        let toks = lex.lex().unwrap();
        check_tok_info(toks, expected);
    }

    #[test]
    fn test_empty() {
        test_lexer("", vec![TokInfo::EOF])
    }

    #[test]
    fn test_keyword() {
        test_lexer("function", vec![TokInfo::Function, TokInfo::EOF])
    }

    #[test]
    fn test_arrow() {
        test_lexer(
            "y=0->pre",
            vec![
                TokInfo::Ident("y"),
                TokInfo::Equal,
                TokInfo::IConst(0),
                TokInfo::Arrow,
                TokInfo::Pre,
                TokInfo::EOF,
            ],
        )
    }

    #[test]
    fn test_keywords() {
        test_lexer(
            "extern function",
            vec![TokInfo::Extern, TokInfo::Function, TokInfo::EOF],
        );
        test_lexer(
            "functional",
            vec![TokInfo::Ident("functional"), TokInfo::EOF],
        );
    }

    #[test]
    fn test_spaces() {
        test_lexer(
            "extern\n  \t\r\nfunction",
            vec![TokInfo::Extern, TokInfo::Function, TokInfo::EOF],
        );
        test_lexer(
            "\n  \t\r\nextern function",
            vec![TokInfo::Extern, TokInfo::Function, TokInfo::EOF],
        );
        test_lexer(
            "extern function\n  \t\r\n",
            vec![TokInfo::Extern, TokInfo::Function, TokInfo::EOF],
        );
    }

    #[test]
    fn test_iconst() {
        test_lexer(
            "42 -12",
            vec![
                TokInfo::IConst(42),
                TokInfo::Minus,
                TokInfo::IConst(12),
                TokInfo::EOF,
            ],
        )
    }

    #[test]
    fn test_rconst() {
        test_lexer("33.3", vec![TokInfo::RConst(33.3), TokInfo::EOF])
    }

    #[test]
    fn test_str() {
        test_lexer(
            "include \"memoire.lus\"",
            vec![TokInfo::Include, TokInfo::Str("memoire.lus"), TokInfo::EOF],
        );
    }

    #[test]
    fn test_comments() {
        test_lexer(
            "-- comment\nfunction\nfunction --comment",
            vec![TokInfo::Function, TokInfo::Function, TokInfo::EOF],
        );
        test_lexer(
            "include (* hello *) extern /* world */ function",
            vec![
                TokInfo::Include,
                TokInfo::Extern,
                TokInfo::Function,
                TokInfo::EOF,
            ],
        )
    }

    #[test]
    fn test_ops() {
        test_lexer(
            "12 + 3",
            vec![
                TokInfo::IConst(12),
                TokInfo::Plus,
                TokInfo::IConst(3),
                TokInfo::EOF,
            ],
        );
        test_lexer(
            "42*7",
            vec![
                TokInfo::IConst(42),
                TokInfo::Star,
                TokInfo::IConst(7),
                TokInfo::EOF,
            ],
        );
    }

    #[test]
    fn test_const_slice() {
        test_lexer(
            "a[1..2]",
            vec![
                TokInfo::Ident("a"),
                TokInfo::OpenBracket,
                TokInfo::IConst(1),
                TokInfo::CDots,
                TokInfo::IConst(2),
                TokInfo::CloseBracket,
                TokInfo::EOF,
            ],
        )
    }

    #[test]
    fn test_ident() {
        test_lexer(
            "a aaaa",
            vec![TokInfo::Ident("a"), TokInfo::Ident("aaaa"), TokInfo::EOF],
        )
    }

    #[test]
    fn test_static_pars() {
        test_lexer(
            "function a<<const n : int>>()",
            vec![
                TokInfo::Function,
                TokInfo::Ident("a"),
                TokInfo::OpenStaticPar,
                TokInfo::Const,
                TokInfo::Ident("n"),
                TokInfo::Colon,
                TokInfo::Int,
                TokInfo::CloseStaticPar,
                TokInfo::OpenPar,
                TokInfo::ClosePar,
                TokInfo::EOF,
            ],
        );
        test_lexer(
            "x + amaury_n<<n-1>>(x);",
            vec![
                TokInfo::Ident("x"),
                TokInfo::Plus,
                TokInfo::Ident("amaury_n"),
                TokInfo::OpenStaticPar,
                TokInfo::Ident("n"),
                TokInfo::Minus,
                TokInfo::IConst(1),
                TokInfo::CloseStaticPar,
                TokInfo::OpenPar,
                TokInfo::Ident("x"),
                TokInfo::ClosePar,
                TokInfo::Semicolon,
                TokInfo::EOF,
            ],
        )
    }

    #[test]
    fn test_unclosed_str() {
        let mut lexer = Lexer::new("main.lus", "\"hello ");
        assert_eq!(lexer.lex(), Err(Error::UnclosedStr));
    }

    #[test]
    fn test_unclosed_comment() {
        let mut lexer = Lexer::new("main.lus", "/* hello ");
        assert_eq!(lexer.lex(), Err(Error::UnclosedComment));
    }

    #[test]
    fn test_amaury() {
        use TokInfo::*;
        let amaury1 = r#"y = with n = 0 then 0 else
         x + amaury_n<<n-1>>(x);"#;
        test_lexer(
            amaury1,
            vec![
                Ident("y"),
                Equal,
                With,
                Ident("n"),
                Equal,
                IConst(0),
                Then,
                IConst(0),
                Else,
                Ident("x"),
                Plus,
                Ident("amaury_n"),
                OpenStaticPar,
                Ident("n"),
                Minus,
                IConst(1),
                CloseStaticPar,
                OpenPar,
                Ident("x"),
                ClosePar,
                Semicolon,
                EOF,
            ],
        );

        let amaury = r#"
node amaury_n<<const n:int>>(x: int) returns (y:int);
let
   y = with n = 0 then 0 else
         x + amaury_n<<n-1>>(x);
tel

node amaury = amaury_n<<4>>;
"#;
        test_lexer(
            amaury,
            vec![
                Node,
                Ident("amaury_n"),
                OpenStaticPar,
                Const,
                Ident("n"),
                Colon,
                Int,
                CloseStaticPar,
                OpenPar,
                Ident("x"),
                Colon,
                Int,
                ClosePar,
                Returns,
                OpenPar,
                Ident("y"),
                Colon,
                Int,
                ClosePar,
                Semicolon,
                Let,
                Ident("y"),
                Equal,
                With,
                Ident("n"),
                Equal,
                IConst(0),
                Then,
                IConst(0),
                Else,
                Ident("x"),
                Plus,
                Ident("amaury_n"),
                OpenStaticPar,
                Ident("n"),
                Minus,
                IConst(1),
                CloseStaticPar,
                OpenPar,
                Ident("x"),
                ClosePar,
                Semicolon,
                Tel,
                Node,
                Ident("amaury"),
                Equal,
                Ident("amaury_n"),
                OpenStaticPar,
                IConst(4),
                CloseStaticPar,
                Semicolon,
                EOF,
            ],
        );
    }
}
