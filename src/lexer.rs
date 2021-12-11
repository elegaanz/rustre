use std::str::FromStr;

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Location {
    line: u64,
    col: u64,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Span<'f> {
    file: &'f str,
    start: Location,
    end: Location,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Tok<'a, 'f> {
    span: Span<'f>,
    tok: TokInfo<'a>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
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

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Error {
    UnclosedStr,
    UnclosedComment,
    UnexpectedEOF,
}

pub struct Lexer<'a, 'f> {
    file: &'f str,
    line: u64,
    col: u64,
    src: &'a str,
    pos: usize,
}

impl<'a, 'f> Lexer<'a, 'f> {
    pub fn new(file: &'f str, src: &'a str) -> Self {
        Lexer {
            file,
            src,
            line: 1,
            col: 0,
            pos: 0,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Tok>, Error> {
        let mut tokens = Vec::with_capacity(self.src.len() / 4);

        let mut tok_start = 0;
        let mut grammar = Grammar::Main;
        let total_len = self.src.len();
        let mut done = total_len == 0;
        while !done {
            done = self.pos >= total_len;

            let len = (self.pos - tok_start) as u64;
            match grammar {
                Grammar::Str => {
                    if self.current() == "\"" {
                        grammar = Grammar::Main;
                        tokens.push(self.token(
                            len as u64,
                            TokInfo::Str(&self.src[tok_start + 1..self.pos]),
                        ));
                        self.advance()?;
                        tok_start = self.pos;
                    } else {
                        self.advance()?;
                    }
                },
                Grammar::InlineComment => {
                    if self.current() == "\n" {
                        grammar = Grammar::Main;
                        self.advance()?;
                        tok_start = self.pos;
                    } else {
                        self.advance()?;
                    }
                },
                Grammar::Main => {
                    let tok_str = &self.src[tok_start..self.pos];
                    if tok_str == "--" {
                        grammar = Grammar::InlineComment;
                        self.advance()?;
                    } else if tok_str == "/*" {
                        grammar = Grammar::Comment('/');
                        self.advance()?;
                    } else if tok_str == "(*" {
                        grammar = Grammar::Comment(')');
                        self.advance()?;
                    } else if self.current() == "\"" {
                        tok_start = self.pos;
                        self.advance()?;
                        grammar = Grammar::Str;
                    } else if !self.current().chars().any(char::is_alphanumeric) {
                        let next = if self.pos + 2 < total_len {
                            Some(&self.src[self.pos + 1..self.pos + 2])
                        } else {
                            None
                        };
                        let (kw, _) = self.find_keyword(len, next, tok_str);
                        let (op, should_reset) = self.find_keyword(1, next, self.current());

                        let mut ch = tok_str.chars();
                        let is_ident = ch.next().map(char::is_alphabetic).unwrap_or(false) && ch.all(|x| x.is_alphanumeric() || x == '_');

                        if let Some(kw) = kw {
                            tokens.push(kw);
                        } else if let Ok(i) = i64::from_str(tok_str) {
                            tokens.push(self.token(len, TokInfo::IConst(i)));
                        } else if let Ok(r) = f64::from_str(tok_str) {
                            tokens.push(self.token(len, TokInfo::RConst(r)));
                        } else if is_ident {
                            tokens.push(self.token(len, TokInfo::Ident(tok_str)));
                        }

                        if let Some(op) = op {
                            tokens.push(op);
                        }
                        self.advance()?;
                        if should_reset {
                            tok_start = self.pos;
                        } else {
                            self.advance()?;
                        }
                    } else {
                        self.advance()?;
                    }
                },
                Grammar::Comment(end) => {
                    if &self.src[self.pos - 2..self.pos - 1] == "*" && self.src[self.pos - 1..self.pos].chars().next() == Some(end) {
                        grammar = Grammar::Main;
                    }
                    self.advance()?;
                    tok_start = self.pos;
                },
            }
        }

        match grammar {
            Grammar::Main | Grammar::InlineComment => {
                tokens.push(self.token(0, TokInfo::EOF));
                Ok(tokens)
            }
            Grammar::Str => Err(Error::UnclosedStr),
            Grammar::Comment(_) => Err(Error::UnclosedComment),
        }
    }

    fn current(&self) -> &'a str {
        if self.pos < self.src.len() {
            &self.src[self.pos..self.pos + 1]
        } else {
            ""
        }
    }

    fn advance(&mut self) -> Result<(), Error> {
        let len = self.src.len();
        if self.pos >= len {
            return Ok(());
        }

        if &self.src[self.pos..self.pos + 1] == "\n" {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.pos += 1;
        if self.pos <= len {
            Ok(())
        } else {
            Err(Error::UnexpectedEOF)
        }
    }

    fn token(&self, len: u64, info: TokInfo<'a>) -> Tok<'a, 'f> {
        Tok {
            span: Span {
                file: self.file,
                start: Location {
                    line: self.line,
                    col: self.col - len,
                },
                end: Location {
                    line: self.line,
                    col: self.col,
                },
            },
            tok: info,
        }
    }

    fn find_keyword(&self, len: u64, next: Option<&'a str>, tok_str: &'a str) -> (Option<Tok<'a, 'f>>, bool) {
        let mut should_reset = true;
        let t = match tok_str {
            "extern" => Some(self.token(len, TokInfo::Extern)),
            "unsafe" => Some(self.token(len, TokInfo::Unsafe)),
            "and" => Some(self.token(len, TokInfo::And)),
            "assert" => Some(self.token(len, TokInfo::Assert)),
            "bool" => Some(self.token(len, TokInfo::Bool)),
            "const" => Some(self.token(len, TokInfo::Const)),
            "current" => Some(self.token(len, TokInfo::Current)),
            "div" => Some(self.token(len, TokInfo::Div)),
            "else" => Some(self.token(len, TokInfo::Else)),
            "enum" => Some(self.token(len, TokInfo::Enum)),
            "function" => Some(self.token(len, TokInfo::Function)),
            "false" => Some(self.token(len, TokInfo::False)),
            "if" => Some(self.token(len, TokInfo::If)),
            "int" => Some(self.token(len, TokInfo::Int)),
            "let" => Some(self.token(len, TokInfo::Let)),
            "mod" => Some(self.token(len, TokInfo::Mod)),
            "node" => Some(self.token(len, TokInfo::Node)),
            "not" => Some(self.token(len, TokInfo::Not)),
            "operator" => Some(self.token(len, TokInfo::Operator)),
            "or" => Some(self.token(len, TokInfo::Or)),
            "nor" => Some(self.token(len, TokInfo::Nor)),
            "fby" => Some(self.token(len, TokInfo::FBy)),
            "pre" => Some(self.token(len, TokInfo::Pre)),
            "real" => Some(self.token(len, TokInfo::Real)),
            "returns" => Some(self.token(len, TokInfo::Returns)),
            "step" => Some(self.token(len, TokInfo::Step)),
            "struct" => Some(self.token(len, TokInfo::Struct)),
            "tel" => Some(self.token(len, TokInfo::Tel)),
            "type" => Some(self.token(len, TokInfo::Type)),
            "then" => Some(self.token(len, TokInfo::Then)),
            "true" => Some(self.token(len, TokInfo::True)),
            "var" => Some(self.token(len, TokInfo::Var)),
            "when" => Some(self.token(len, TokInfo::When)),
            "with" => Some(self.token(len, TokInfo::With)),
            "xor" => Some(self.token(len, TokInfo::Xor)),
            "model" => Some(self.token(len, TokInfo::Model)),
            "package" => Some(self.token(len, TokInfo::Package)),
            "needs" => Some(self.token(len, TokInfo::Needs)),
            "provides" => Some(self.token(len, TokInfo::Provides)),
            "uses" => Some(self.token(len, TokInfo::Uses)),
            "is" => Some(self.token(len, TokInfo::Is)),
            "body" => Some(self.token(len, TokInfo::Body)),
            "end" => Some(self.token(len, TokInfo::End)),
            "include" => Some(self.token(len, TokInfo::Include)),
            "merge" => Some(self.token(len, TokInfo::Merge)),
            "->" => Some(self.token(len, TokInfo::Arrow)),
            "=>" => Some(self.token(len, TokInfo::Impl)),
            "<=" => Some(self.token(len, TokInfo::Lte)),
            "<>" => Some(self.token(len, TokInfo::Neq)),
            ">=" => Some(self.token(len, TokInfo::Gte)),
            ".." => Some(self.token(len, TokInfo::CDots)),
            "**" => Some(self.token(len, TokInfo::Power)),
            "<<" => Some(self.token(len, TokInfo::OpenStaticPar)),
            ">>" => Some(self.token(len, TokInfo::CloseStaticPar)),
            "+" => Some(self.token(len, TokInfo::Plus)),
            "^" => Some(self.token(len, TokInfo::Hat)),
            "#" => Some(self.token(len, TokInfo::Sharp)),
            "-" => if next != Some("-") { Some(self.token(len, TokInfo::Minus)) } else { should_reset = false; None },
            "/" => if next != Some("*") { Some(self.token(len, TokInfo::Slash)) } else { should_reset = false; None },
            "%" => Some(self.token(len, TokInfo::Percent)),
            "*" => Some(self.token(len, TokInfo::Star)),
            "|" => Some(self.token(len, TokInfo::Bar)),
            "=" => Some(self.token(len, TokInfo::Equal)),
            "." => Some(self.token(len, TokInfo::Dot)),
            "," => Some(self.token(len, TokInfo::Coma)),
            ";" => Some(self.token(len, TokInfo::Semicolon)),
            ":" => Some(self.token(len, TokInfo::Colon)),
            "(" => if next != Some("*") { Some(self.token(len, TokInfo::OpenPar)) } else { should_reset = false; None },
            ")" => Some(self.token(len, TokInfo::ClosePar)),
            "{" => Some(self.token(len, TokInfo::OpenBrace)),
            "}" => Some(self.token(len, TokInfo::CloseBrace)),
            "[" => Some(self.token(len, TokInfo::OpenBracket)),
            "]" => Some(self.token(len, TokInfo::CloseBracket)),
            "<" => Some(self.token(len, TokInfo::Lt)),
            ">" => Some(self.token(len, TokInfo::Gt)),
            _ => None,
        };
        (t, should_reset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_tok_info<'a, 'f>(actual: Vec<Tok<'a, 'f>>, expected: Vec<TokInfo<'a>>) {
        assert_eq!(actual.len(), expected.len());

        for (ref a, ref e) in actual.iter().zip(expected.iter()) {
            assert_eq!(a.tok, **e);
        }
    }


    fn test_lexer<'a>(src: &'a str, expected: Vec<TokInfo<'a>>) {
        let mut lex = Lexer::new("main.lus", src);
        let toks = lex.lex().unwrap();
        check_tok_info(toks, expected);
    }

    #[test]
    fn test_empty() {
        test_lexer("", vec![ TokInfo::EOF ])
    }

    #[test]
    fn test_keyword() {
        test_lexer("function", vec![ TokInfo::Function, TokInfo::EOF ])
    }
    
    #[test]
    fn test_keywords() {
        test_lexer("extern function", vec![ TokInfo::Extern, TokInfo::Function, TokInfo::EOF ]);
        test_lexer("functional", vec![ TokInfo::Ident("functional"), TokInfo::EOF ]);
    }

    #[test]
    fn test_spaces() {
        test_lexer("extern\n  \t\r\nfunction", vec![ TokInfo::Extern, TokInfo::Function, TokInfo::EOF ]);
        test_lexer("\n  \t\r\nextern function", vec![ TokInfo::Extern, TokInfo::Function, TokInfo::EOF ]);
        test_lexer("extern function\n  \t\r\n", vec![ TokInfo::Extern, TokInfo::Function, TokInfo::EOF ]);
    }

    #[test]
    fn test_iconst() {
        test_lexer("42 -12", vec![ TokInfo::IConst(42), TokInfo::Minus, TokInfo::IConst(12), TokInfo::EOF ])
    }

    #[test]
    fn test_str() {
        test_lexer("include \"memoire.lus\"", vec![ TokInfo::Include, TokInfo::Str("memoire.lus"), TokInfo::EOF ]);
    }

    #[test]
    fn test_comments() {
        test_lexer("-- comment\nfunction\nfunction --comment", vec![ TokInfo::Function, TokInfo::Function, TokInfo::EOF ]);
        test_lexer("include (* hello *) extern /* world */ function", vec![ TokInfo::Include, TokInfo::Extern, TokInfo::Function, TokInfo::EOF ])
    }

    #[test]
    fn test_ops() {
        test_lexer("12 + 3", vec![ TokInfo::IConst(12), TokInfo::Plus, TokInfo::IConst(3), TokInfo::EOF ]);
        test_lexer("42*7", vec![ TokInfo::IConst(42), TokInfo::Star, TokInfo::IConst(7), TokInfo::EOF ]);
    }
}
