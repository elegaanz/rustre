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

impl<'a, 'f> Lexer<'a, 'f> {
    pub fn new(file: &'f str, src: &'a str) -> Self {
        Lexer { file, src }
    }

    pub fn lex(&mut self) -> Result<Vec<Tok>, Error> {
        let total_len = self.src.len();
        let mut tokens = Vec::with_capacity(self.src.len() / 4);
        let mut end = self.find_next_delimiter(0);
        let mut grammar = Grammar::Main;
        let mut pos = 0;
        let mut line = 1;
        let mut col = 0;
        while pos < total_len {
            if total_len > 5000 {
                // i know the lexer is slow so here is a progress bar
                print!("\r {} / {}", pos, total_len);
            }
            match grammar {
                Grammar::Main => {
                    let tok_str = &self.src[pos..end];
                    if tok_str == "\"" {
                        grammar = Grammar::Str;
                    } else if tok_str == "--" {
                        grammar = Grammar::InlineComment;
                    } else if tok_str == "/*" {
                        grammar = Grammar::Comment('/');
                    } else if tok_str == "(*" {
                        grammar = Grammar::Comment(')');
                    } else {
                        if let Some(tok) = Self::match_tok(self.file, line, col, pos, tok_str) {
                            tokens.push(tok);
                            col += (end - pos) as u64;
                            pos = end;
                            end = self.find_next_delimiter(pos);
                        } else {
                            end -= 1;
                        }
                    }
                }
                Grammar::Str => {
                    let mut str_end = pos + 1;
                    let original_line = line;
                    let original_col = col;
                    while str_end + 1 < total_len && &self.src[str_end..str_end + 1] != "\"" {
                        match &self.src[str_end..str_end + 1] {
                            "\n" => {
                                col = 0;
                                line += 1
                            }
                            _ => col += 1,
                        }
                        str_end += 1;
                    }
                    tokens.push(Spanned {
                        span: Span {
                            file: self.file,
                            start: Location {
                                line: original_line,
                                col: original_col,
                                pos: (pos + 1) as u64,
                            },
                            end: Location {
                                line,
                                col,
                                pos: str_end as u64,
                            },
                        },
                        item: TokInfo::Str(&self.src[pos + 1..str_end]),
                    });
                    pos = str_end + 1;
                    end = self.find_next_delimiter(pos);
                    if pos < total_len || &self.src[str_end..str_end + 1] == "\"" {
                        grammar = Grammar::Main;
                    }
                }
                Grammar::Comment(end) => {
                    let mut comm_end = pos + 1;
                    while comm_end + 1 < total_len
                        && &self.src[comm_end..comm_end + 2] != &format!("*{}", end)
                    {
                        match &self.src[comm_end..comm_end + 1] {
                            "\n" => {
                                col = 0;
                                line += 1
                            }
                            _ => col += 1,
                        }
                        comm_end += 1;
                    }
                    pos = comm_end + 1;
                    if pos < total_len {
                        grammar = Grammar::Main;
                    }
                }
                Grammar::InlineComment => {
                    let mut comm_end = pos + 1;
                    while comm_end + 1 < total_len && &self.src[comm_end..comm_end + 1] != "\n" {
                        match &self.src[comm_end..comm_end + 1] {
                            "\n" => {
                                col = 0;
                                line += 1
                            }
                            _ => col += 1,
                        }
                        comm_end += 1;
                    }
                    pos = comm_end;
                    grammar = Grammar::Main;
                }
            }

            if pos >= end {
                if pos + 1 < total_len {
                    match &self.src[pos..pos + 1] {
                        "\n" => {
                            col = 0;
                            line += 1
                        }
                        _ => col += 1,
                    };
                }
                pos += 1;
                end = total_len;
            }
        }
        match grammar {
            Grammar::Main | Grammar::InlineComment => {
                tokens.push(Spanned {
                    span: Span {
                        file: self.file,
                        start: Location {
                            line,
                            col,
                            pos: pos as u64,
                        },
                        end: Location {
                            line,
                            col,
                            pos: pos as u64,
                        },
                    },
                    item: TokInfo::EOF,
                });
                Ok(tokens)
            }
            Grammar::Comment(_) => Err(Error::UnclosedComment),
            Grammar::Str => Err(Error::UnclosedStr),
        }
    }

    fn match_tok(
        file: &'f str,
        line: u64,
        col: u64,
        pos: usize,
        src: &'a str,
    ) -> Option<Spanned<'f, TokInfo<'a>>> {
        let len = src.len() as u64;
        let pos = (pos as u64) + len;
        let col = col + len;
        match src {
            "," => Some(Self::token(file, line, col, pos, len, TokInfo::Coma)),
            ";" => Some(Self::token(file, line, col, pos, len, TokInfo::Semicolon)),
            "::" => Some(Self::token(file, line, col, pos, len, TokInfo::DoubleColon)),
            ":" => Some(Self::token(file, line, col, pos, len, TokInfo::Colon)),
            "->" => Some(Self::token(file, line, col, pos, len, TokInfo::Arrow)),
            "=>" => Some(Self::token(file, line, col, pos, len, TokInfo::Impl)),
            "<=" => Some(Self::token(file, line, col, pos, len, TokInfo::Lte)),
            "<>" => Some(Self::token(file, line, col, pos, len, TokInfo::Neq)),
            ">=" => Some(Self::token(file, line, col, pos, len, TokInfo::Gte)),
            ".." => Some(Self::token(file, line, col, pos, len, TokInfo::CDots)),
            "**" => Some(Self::token(file, line, col, pos, len, TokInfo::Power)),
            "<<" => Some(Self::token(
                file,
                line,
                col,
                pos,
                len,
                TokInfo::OpenStaticPar,
            )),
            ">>" => Some(Self::token(
                file,
                line,
                col,
                pos,
                len,
                TokInfo::CloseStaticPar,
            )),
            "+" => Some(Self::token(file, line, col, pos, len, TokInfo::Plus)),
            "^" => Some(Self::token(file, line, col, pos, len, TokInfo::Hat)),
            "#" => Some(Self::token(file, line, col, pos, len, TokInfo::Sharp)),
            "-" => Some(Self::token(file, line, col, pos, len, TokInfo::Minus)),
            "/" => Some(Self::token(file, line, col, pos, len, TokInfo::Slash)),
            "%" => Some(Self::token(file, line, col, pos, len, TokInfo::Percent)),
            "*" => Some(Self::token(file, line, col, pos, len, TokInfo::Star)),
            "|" => Some(Self::token(file, line, col, pos, len, TokInfo::Bar)),
            "=" => Some(Self::token(file, line, col, pos, len, TokInfo::Equal)),
            "." => Some(Self::token(file, line, col, pos, len, TokInfo::Dot)),
            "(" => Some(Self::token(file, line, col, pos, len, TokInfo::OpenPar)),
            ")" => Some(Self::token(file, line, col, pos, len, TokInfo::ClosePar)),
            "{" => Some(Self::token(file, line, col, pos, len, TokInfo::OpenBrace)),
            "}" => Some(Self::token(file, line, col, pos, len, TokInfo::CloseBrace)),
            "[" => Some(Self::token(file, line, col, pos, len, TokInfo::OpenBracket)),
            "]" => Some(Self::token(
                file,
                line,
                col,
                pos,
                len,
                TokInfo::CloseBracket,
            )),
            "<" => Some(Self::token(file, line, col, pos, len, TokInfo::Lt)),
            ">" => Some(Self::token(file, line, col, pos, len, TokInfo::Gt)),
            "extern" => Some(Self::token(file, line, col, pos, len, TokInfo::Extern)),
            "unsafe" => Some(Self::token(file, line, col, pos, len, TokInfo::Unsafe)),
            "and" => Some(Self::token(file, line, col, pos, len, TokInfo::And)),
            "assert" => Some(Self::token(file, line, col, pos, len, TokInfo::Assert)),
            "bool" => Some(Self::token(file, line, col, pos, len, TokInfo::Bool)),
            "const" => Some(Self::token(file, line, col, pos, len, TokInfo::Const)),
            "current" => Some(Self::token(file, line, col, pos, len, TokInfo::Current)),
            "div" => Some(Self::token(file, line, col, pos, len, TokInfo::Div)),
            "else" => Some(Self::token(file, line, col, pos, len, TokInfo::Else)),
            "enum" => Some(Self::token(file, line, col, pos, len, TokInfo::Enum)),
            "function" => Some(Self::token(file, line, col, pos, len, TokInfo::Function)),
            "false" => Some(Self::token(file, line, col, pos, len, TokInfo::False)),
            "if" => Some(Self::token(file, line, col, pos, len, TokInfo::If)),
            "int" => Some(Self::token(file, line, col, pos, len, TokInfo::Int)),
            "let" => Some(Self::token(file, line, col, pos, len, TokInfo::Let)),
            "mod" => Some(Self::token(file, line, col, pos, len, TokInfo::Mod)),
            "node" => Some(Self::token(file, line, col, pos, len, TokInfo::Node)),
            "not" => Some(Self::token(file, line, col, pos, len, TokInfo::Not)),
            "operator" => Some(Self::token(file, line, col, pos, len, TokInfo::Operator)),
            "or" => Some(Self::token(file, line, col, pos, len, TokInfo::Or)),
            "nor" => Some(Self::token(file, line, col, pos, len, TokInfo::Nor)),
            "fby" => Some(Self::token(file, line, col, pos, len, TokInfo::FBy)),
            "pre" => Some(Self::token(file, line, col, pos, len, TokInfo::Pre)),
            "real" => Some(Self::token(file, line, col, pos, len, TokInfo::Real)),
            "returns" => Some(Self::token(file, line, col, pos, len, TokInfo::Returns)),
            "step" => Some(Self::token(file, line, col, pos, len, TokInfo::Step)),
            "struct" => Some(Self::token(file, line, col, pos, len, TokInfo::Struct)),
            "tel" => Some(Self::token(file, line, col, pos, len, TokInfo::Tel)),
            "type" => Some(Self::token(file, line, col, pos, len, TokInfo::Type)),
            "then" => Some(Self::token(file, line, col, pos, len, TokInfo::Then)),
            "true" => Some(Self::token(file, line, col, pos, len, TokInfo::True)),
            "var" => Some(Self::token(file, line, col, pos, len, TokInfo::Var)),
            "when" => Some(Self::token(file, line, col, pos, len, TokInfo::When)),
            "with" => Some(Self::token(file, line, col, pos, len, TokInfo::With)),
            "xor" => Some(Self::token(file, line, col, pos, len, TokInfo::Xor)),
            "model" => Some(Self::token(file, line, col, pos, len, TokInfo::Model)),
            "package" => Some(Self::token(file, line, col, pos, len, TokInfo::Package)),
            "needs" => Some(Self::token(file, line, col, pos, len, TokInfo::Needs)),
            "provides" => Some(Self::token(file, line, col, pos, len, TokInfo::Provides)),
            "uses" => Some(Self::token(file, line, col, pos, len, TokInfo::Uses)),
            "is" => Some(Self::token(file, line, col, pos, len, TokInfo::Is)),
            "body" => Some(Self::token(file, line, col, pos, len, TokInfo::Body)),
            "end" => Some(Self::token(file, line, col, pos, len, TokInfo::End)),
            "include" => Some(Self::token(file, line, col, pos, len, TokInfo::Include)),
            "merge" => Some(Self::token(file, line, col, pos, len, TokInfo::Merge)),
            x if x.chars().all(char::is_numeric) => Some(Self::token(
                file,
                line,
                col,
                pos,
                len,
                TokInfo::IConst(x.parse::<i64>().unwrap()),
            )),
            x if x
                .chars()
                .all(|c| c.is_numeric() || c == '.' || c == 'e' || c == 'E')
                && x.chars().any(char::is_numeric)
                && x.parse::<f64>().is_ok() =>
            {
                Some(Self::token(
                    file,
                    line,
                    col,
                    pos,
                    len,
                    TokInfo::RConst(x.parse::<f64>().unwrap()),
                ))
            }
            x if x.chars().all(|c| c.is_alphanumeric() || c == '_') => {
                Some(Self::token(file, line, col, pos, len, TokInfo::Ident(x)))
            }
            _ => None,
        }
    }

    fn token(
        file: &'f str,
        line: u64,
        col: u64,
        pos: u64,
        len: u64,
        info: TokInfo<'a>,
    ) -> Tok<'a, 'f> {
        Spanned {
            span: Span {
                file,
                start: Location {
                    line,
                    col: col - len,
                    pos: pos - len,
                },
                end: Location { line, col, pos },
            },
            item: info,
        }
    }

    fn find_next_delimiter(&self, pos: usize) -> usize {
        let src = &self.src[pos..];
        let mut delim = self.src.len();
        for pat in [" ", "\t", "\n", "--", "/*", "(*", "\""] {
            if let Some(pat_pos) = src.find(pat) {
                // 10 = arbitrary limit considered short enough
                if pat_pos < 10 {
                    return pos + pat_pos + pat.len();
                }
                if pat_pos + pat.len() < delim {
                    delim = pos + pat_pos + pat.len();
                }
            }
        }
        delim
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
