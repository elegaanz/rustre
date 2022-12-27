use logos::{Lexer, Logos, SpannedIter};

#[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
/// A token
#[repr(u16)]
pub enum Token {
    /// Whitespace character
    ///
    /// Includes: spaces, tabs, newlines, carriage returns, form feeds
    #[regex("[ \t\n\r\u{0C}]")]
    Space,

    #[token("extern")]
    Extern,

    #[token("unsafe")]
    Unsafe,

    #[token("and")]
    And,

    #[token("->")]
    Arrow,

    #[token("assert")]
    Assert,

    #[token("|")]
    Bar,

    #[token("bool")]
    Bool,

    #[token("..")]
    CDots,

    #[token("}")]
    CloseBrace,

    #[token("]")]
    CloseBracket,

    #[token(")")]
    ClosePar,

    #[token(">>")]
    CloseStaticPar,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("const")]
    Const,

    #[token("current")]
    Current,

    #[token("#")]
    Diese,

    #[token("div")]
    Div,

    #[token("::")]
    DoubleColon,

    #[token(".")]
    Dot,

    #[token("=")]
    Equal,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    #[token("false")]
    False,

    #[token("function")]
    Function,

    #[token(">")]
    Gt,

    #[token(">=")]
    Gte,

    #[token("^")]
    Hat,

    #[regex(r"\d+")]
    IConst,

    /// A very special case to handle an [IConst][Token::IConst] directly followed by a `..`
    /// ([CDots][Token::CDots]) operator.
    ///
    /// This specialization is important to be able to parse [RConst][Token::RConst] properly, or
    /// else `1..2` would be parsed as `1.` `.` `2` which is a syntax error and not what's supposed
    /// to be parsed in the first place.
    ///
    /// This weird trick of treating two tokens as one could be avoided if [`logos`] supported
    /// regex lookaheads, so [RConst][Token::RConst] could reject a list of digits followed by 2
    /// dots.
    #[regex(r"\d+\.\.", priority = 11)]
    IConstAndCDots,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")] // TODO: check
    Ident,

    #[token("if")]
    If,

    #[token("=>")]
    Impl,

    #[token("int")]
    Int,

    #[token("let")]
    Let,

    #[token("<")]
    Lt,

    #[token("<=")]
    Lte,

    #[token("merge")]
    Merge,

    #[token("-")]
    Minus,

    #[token("mod")]
    Mod,

    #[token("<>")]
    Neq,

    #[token("node")]
    Node,

    #[token("nor")]
    Nor,

    #[token("not")]
    Not,

    #[token("{")]
    OpenBrace,

    #[token("[")]
    OpenBracket,

    #[token("(")]
    OpenPar,

    #[token("<<")]
    OpenStaticPar,

    #[token("operator")]
    Operator,

    #[token("or")]
    Or,

    #[token("%")]
    Percent,

    #[token("+")]
    Plus,

    #[token("**")]
    Power,

    #[token("pre")]
    Pre,

    #[token("fby")]
    FBy,

    /// Recognizes a floating-point literal
    ///
    /// The regex is intentionally a bit greedy, but this gives room for better errors if the value
    /// is unparseable.
    #[regex(r"\d+\.\d*(e[+-]?\d+)?")]
    RConst,

    #[token("real")]
    Real,

    #[token("returns")]
    Returns,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("step")]
    Step,

    #[token("struct")]
    Struct,

    #[token("tel")]
    Tel,

    #[token("then")]
    Then,

    #[token("true")]
    True,

    #[token("type")]
    Type,

    #[token("var")]
    Var,

    #[token("when")]
    When,

    #[token("with")]
    With,

    #[token("xor")]
    Xor,

    #[token("model")]
    Model,

    #[token("package")]
    Package,

    #[token("needs")]
    Needs,

    #[token("provides")]
    Provides,

    #[token("uses")]
    Uses,

    #[token("is")]
    Is,

    #[token("body")]
    Body,

    #[token("end")]
    End,

    #[token("include")]
    Include,

    #[regex(r#""([^"]|\\")*""#)]
    Str,

    #[regex(r"--.*\n")]
    InlineComment,

    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    #[regex(r"\(\*([^*]|\*[^\)])*\*\)")]
    Comment,

    #[error]
    Error,

    // Composite nodes

    // Ebnf group ProgramRules
    Root,

    /// Children: Include + String
    IncludeStatement,

    // Ebnf group PackageRules
    PackageDeclNode,
    PackageDeclBody,
    PackageAliasNode,
    UsesNode,

    // Ebnf group ModelRules
    ProvidesNode,
    ModelDeclNode,

    // Ebnf group IdentRules
    IdNode,
    PragmaNode,

    // Ebnf group NodesRules
    TypedLv6IdsNode,
    TypedValuedLv6IdNode,
    NodeNode,
    ParamsNode,

    // Ebnf group ConstantDeclRules
    ConstantDeclNode,
    OneConstantDeclNode,

    // Ebnf group TypeDeclRules
    TypeDeclNode,
    OneTypeDeclNode,
    EnumDeclNode,
    StructDeclNode,

    // Ebnf group SimpleTypeRules
    TypeNode,

    // Ebnf group ExtNodesRules
    ExternalNodeDeclNode,

    // Ebnf group StaticRules
    StaticParamsNode,
    StaticParamNode,
    EffectiveNodeNode,
    StaticArgsNode,
    StaticArgNode,
    NamedStaticArgsNode,
    NamedStaticArgNode,

    // Ebnf group BodyRules
    BodyNode,
    AssertEquationNode,
    EqualsEquationNode,

    // Ebnf group LeftRules
    LeftNode,
    LeftFieldAccessNode,
    LeftTableAccessNode,
    SelectNode,
    StepNode,

    // Ebnf group ExpressionRules
    ExpressionNode,

    ParExpressionNode,
    ArrayLiteralExpressionNode,
    IdentExpressionNode,

    FbyExpressionNode,

    CallByPosExpressionNode,
    ArrayAccessExpressionNode,

    HatExpressionNode,
    FieldAccessExpressionNode,

    NegExpressionNode,
    PreExpressionNode,
    CurrentExpressionNode,
    DieseExpressionNode,
    NorExpressionNode,

    IntExpressionNode,
    RealExpressionNode,

    WhenExpressionNode,

    PowerExpressionNode,

    MulExpressionNode,
    DivExpressionNode,
    ModExpressionNode,

    AddExpressionNode,
    SubExpressionNode,

    NotExpressionNode,

    LtExpressionNode,
    LteExpressionNode,
    EqExpressionNode,
    GteExpressionNode,
    GtExpressionNode,
    NeqExpressionNode,

    AndExpressionNode,
    OrExpressionNode,
    XorExpressionNode,

    ImplExpressionNode,

    ArrowExpressionNode,

    ConcatExpressionNode,

    IfExpressionNode,

    ClockExpressionNode,

    MergeExpressionNode,

    // Ebnf group MergeRules
    MergeCaseNode,

    // Ebnf group PredefRules
    PredefOp,

    // Ebnf group ExpressionByNamesRules
    CallByNameExpressionNode,
    CallByNameParamNode,

    // Ebnf group ConstantRules
    ConstantNode,
}

pub trait LexerExpansionExt: Iterator + Sized {
    type ExpandedLexerItem: Iterator<Item = Self::Item>;

    fn expand_one(item: Self::Item) -> Self::ExpandedLexerItem;
    fn expanded(
        self,
    ) -> std::iter::FlatMap<Self, Self::ExpandedLexerItem, fn(Self::Item) -> Self::ExpandedLexerItem>
    {
        self.flat_map(Self::expand_one)
    }
}

impl<'source> LexerExpansionExt for Lexer<'source, Token> {
    type ExpandedLexerItem = std::iter::Take<std::array::IntoIter<Self::Item, 2>>;

    fn expand_one(item: Self::Item) -> Self::ExpandedLexerItem {
        match item {
            Token::IConstAndCDots => [Token::IConst, Token::CDots].into_iter().take(2),
            other => [other, Token::Error].into_iter().take(1),
        }
    }
}

impl<'source> LexerExpansionExt for SpannedIter<'source, Token> {
    type ExpandedLexerItem = std::iter::Take<std::array::IntoIter<Self::Item, 2>>;

    fn expand_one(item: Self::Item) -> Self::ExpandedLexerItem {
        match item {
            (Token::IConstAndCDots, span) => {
                let i_const = (Token::IConst, span.start..span.end - 2);
                let c_dots = (Token::CDots, span.end - 2..span.end);
                [i_const, c_dots].into_iter().take(2)
            }
            other => [other, (Token::Error, 0..0)].into_iter().take(1),
        }
    }
}

impl From<Token> for rowan::SyntaxKind {
    fn from(tok: Token) -> Self {
        Self(tok as u16)
    }
}

#[derive(Hash, Ord, PartialOrd, PartialEq, Eq, Debug, Copy, Clone)]
pub enum LustreLang {}
impl rowan::Language for LustreLang {
    type Kind = Token;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<u16, Token>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

impl crate::rowan_nom::RowanNomLanguage for LustreLang {
    fn is_trivia(kind: Self::Kind) -> bool {
        matches!(kind, Token::Comment | Token::InlineComment | Token::Space)
    }

    fn get_error_kind() -> Self::Kind {
        Token::Error
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lexer(src: &str, expected: Vec<Token>) {
        let lex = Token::lexer(src);
        let toks: Vec<_> = lex.collect();
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_empty() {
        test_lexer("", vec![])
    }

    #[test]
    fn test_keyword() {
        test_lexer("function", vec![Token::Function])
    }

    #[test]
    fn test_arrow() {
        test_lexer(
            "y=0->pre",
            vec![
                Token::Ident,
                Token::Equal,
                Token::IConst,
                Token::Arrow,
                Token::Pre,
            ],
        )
    }

    #[test]
    fn test_keywords() {
        test_lexer(
            "extern function",
            vec![Token::Extern, Token::Space, Token::Function],
        );
        test_lexer("functional", vec![Token::Ident]);
    }

    #[test]
    fn test_spaces() {
        test_lexer(
            "extern\n  \t\r\nfunction",
            vec![
                Token::Extern,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Function,
            ],
        );
        test_lexer(
            "\n  \t\r\nextern function",
            vec![
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Extern,
                Token::Space,
                Token::Function,
            ],
        );
        test_lexer(
            "extern function\n  \t\r\n",
            vec![
                Token::Extern,
                Token::Space,
                Token::Function,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
            ],
        );
    }

    #[test]
    fn test_iconst() {
        test_lexer(
            "42 -12",
            vec![Token::IConst, Token::Space, Token::Minus, Token::IConst],
        )
    }

    #[test]
    fn test_rconst() {
        test_lexer("33.3", vec![Token::RConst])
    }

    #[test]
    fn test_str() {
        test_lexer(
            "include \"memoire.lus\"",
            vec![Token::Include, Token::Space, Token::Str],
        );
    }

    #[test]
    fn test_comments() {
        // TODO: a comment followed by EOF is valid, but Logos cannot handle that with regexes
        test_lexer(
            "-- comment\nfunction\nfunction --comment\n",
            vec![
                Token::InlineComment,
                Token::Function,
                Token::Space,
                Token::Function,
                Token::Space,
                Token::InlineComment,
            ],
        );
        test_lexer(
            "include (* hello *) extern /* world */ function",
            vec![
                Token::Include,
                Token::Space,
                Token::Comment,
                Token::Space,
                Token::Extern,
                Token::Space,
                Token::Comment,
                Token::Space,
                Token::Function,
            ],
        )
    }

    #[test]
    fn test_ops() {
        test_lexer(
            "12 + 3",
            vec![
                Token::IConst,
                Token::Space,
                Token::Plus,
                Token::Space,
                Token::IConst,
            ],
        );
        test_lexer("42*7", vec![Token::IConst, Token::Star, Token::IConst]);
    }

    #[test]
    fn test_const_slice() {
        test_lexer(
            "a[1..2]",
            vec![
                Token::Ident,
                Token::OpenBracket,
                Token::IConstAndCDots,
                Token::IConst,
                Token::CloseBracket,
            ],
        )
    }

    #[test]
    fn test_const_slice_and_step() {
        test_lexer(
            "a[1..2 step 2]",
            vec![
                Token::Ident,
                Token::OpenBracket,
                Token::IConstAndCDots,
                Token::IConst,
                Token::Space,
                Token::Step,
                Token::Space,
                Token::IConst,
                Token::CloseBracket,
            ],
        )
    }

    #[test]
    fn test_ident() {
        test_lexer("a aaaa", vec![Token::Ident, Token::Space, Token::Ident])
    }

    #[test]
    fn test_static_pars() {
        test_lexer(
            "function a<<const n : int>>()",
            vec![
                Token::Function,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::Const,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Colon,
                Token::Space,
                Token::Int,
                Token::CloseStaticPar,
                Token::OpenPar,
                Token::ClosePar,
            ],
        );
        test_lexer(
            "x + amaury_n<<n-1>>(x);",
            vec![
                Token::Ident,
                Token::Space,
                Token::Plus,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::Ident,
                Token::Minus,
                Token::IConst,
                Token::CloseStaticPar,
                Token::OpenPar,
                Token::Ident,
                Token::ClosePar,
                Token::Semicolon,
            ],
        )
    }

    #[test]
    fn test_unclosed_str() {
        test_lexer("\"hello ", vec![Token::Error]);
    }

    #[test]
    fn test_unclosed_comment() {
        test_lexer("/* hello", vec![Token::Error]);
    }

    #[test]
    fn test_amaury() {
        let amaury1 = r#"y = with n = 0 then 0 else
 x + amaury_n<<n-1>>(x);"#;
        test_lexer(
            amaury1,
            vec![
                Token::Ident,
                Token::Space,
                Token::Equal,
                Token::Space,
                Token::With,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Equal,
                Token::Space,
                Token::IConst,
                Token::Space,
                Token::Then,
                Token::Space,
                Token::IConst,
                Token::Space,
                Token::Else,
                Token::Space,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Plus,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::Ident,
                Token::Minus,
                Token::IConst,
                Token::CloseStaticPar,
                Token::OpenPar,
                Token::Ident,
                Token::ClosePar,
                Token::Semicolon,
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
                Token::Space,
                Token::Node,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::Const,
                Token::Space,
                Token::Ident,
                Token::Colon,
                Token::Int,
                Token::CloseStaticPar,
                Token::OpenPar,
                Token::Ident,
                Token::Colon,
                Token::Space,
                Token::Int,
                Token::ClosePar,
                Token::Space,
                Token::Returns,
                Token::Space,
                Token::OpenPar,
                Token::Ident,
                Token::Colon,
                Token::Int,
                Token::ClosePar,
                Token::Semicolon,
                Token::Space,
                Token::Let,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Equal,
                Token::Space,
                Token::With,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Equal,
                Token::Space,
                Token::IConst,
                Token::Space,
                Token::Then,
                Token::Space,
                Token::IConst,
                Token::Space,
                Token::Else,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Plus,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::Ident,
                Token::Minus,
                Token::IConst,
                Token::CloseStaticPar,
                Token::OpenPar,
                Token::Ident,
                Token::ClosePar,
                Token::Semicolon,
                Token::Space,
                Token::Tel,
                Token::Space,
                Token::Space,
                Token::Node,
                Token::Space,
                Token::Ident,
                Token::Space,
                Token::Equal,
                Token::Space,
                Token::Ident,
                Token::OpenStaticPar,
                Token::IConst,
                Token::CloseStaticPar,
                Token::Semicolon,
                Token::Space,
            ],
        );
    }
}
