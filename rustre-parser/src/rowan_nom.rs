mod error;

pub use error::*;
use nom::combinator::map;
use nom::Parser;
use rowan::{GreenNode, GreenToken, NodeOrToken, SyntaxKind, SyntaxNode};
use std::marker::PhantomData;

pub trait RowanNomLanguage: rowan::Language {
    /// Returns `true` if the token is a trivia token (whitespace or comment)
    fn is_trivia(_kind: Self::Kind) -> bool {
        false
    }

    fn get_error_kind() -> Self::Kind;
}

// Yup, that's a trait alias
use RowanNomLanguage as Language;

type RichToken<'src, Lang> = (<Lang as rowan::Language>::Kind, &'src str);

pub struct Input<'slice, 'src, Lang: Language> {
    src_pos: usize,
    trivia_tokens: &'slice [RichToken<'src, Lang>],
    trivia_tokens_text_len: usize,
    tokens: &'slice [RichToken<'src, Lang>],
}

fn split_slice_predicate<T>(slice: &[T], predicate: impl FnMut(&T) -> bool) -> (&[T], &[T]) {
    let non_trivia_idx = slice.iter().position(predicate).unwrap_or(slice.len());

    slice.split_at(non_trivia_idx)
}

impl<'slice, 'src, Lang: Language> Input<'slice, 'src, Lang> {
    /// Advances to the next token
    ///
    /// Panics if the input is empty
    fn next(self) -> Self {
        match self.tokens {
            [] => panic!("empty input"),
            [first, rest @ ..] => Input {
                src_pos: self.src_pos + self.trivia_tokens_text_len + first.1.len(),
                trivia_tokens: &[],
                trivia_tokens_text_len: 0,
                tokens: rest,
            }
            .advance_trivia(),
        }
    }

    fn next_trivia(self) -> Option<(Self, &'slice RichToken<'src, Lang>)> {
        if let [token, rest @ ..] = self.trivia_tokens {
            let token_len = token.1.len();

            Some((
                Input {
                    src_pos: self.src_pos + token_len,
                    trivia_tokens: rest,
                    trivia_tokens_text_len: self.trivia_tokens_text_len - token_len,
                    tokens: self.tokens,
                },
                token,
            ))
        } else {
            None
        }
    }

    fn next_raw(&mut self) {
        if let Some(trivia) = self.trivia_tokens.first() {
            let text_len = trivia.1.len();
            self.src_pos += text_len;
            self.trivia_tokens_text_len -= text_len;
            self.trivia_tokens = &self.trivia_tokens[1..];
        } else if let Some(token) = self.tokens.first() {
            self.src_pos += token.1.len();
            self.tokens = &self.tokens[1..];
        } else {
            panic!("empty input");
        }
    }

    fn advance_trivia(self) -> Self {
        let (trivia_tokens, rest_tokens) =
            split_slice_predicate(self.tokens, |(t, _)| !Lang::is_trivia(*t));

        let trivia_tokens_text_len = trivia_tokens.iter().map(|(_, s)| s.len()).sum();

        Self {
            src_pos: self.src_pos,
            trivia_tokens,
            trivia_tokens_text_len,
            tokens: rest_tokens,
        }
    }
}

impl<'slice, 'src, Lang: Language> Clone for Input<'slice, 'src, Lang> {
    fn clone(&self) -> Self {
        Self {
            src_pos: self.src_pos,
            trivia_tokens: self.trivia_tokens,
            trivia_tokens_text_len: self.trivia_tokens_text_len,
            tokens: self.tokens,
        }
    }
}

impl<'slice, 'src, Lang: Language> From<&'slice [RichToken<'src, Lang>]>
    for Input<'slice, 'src, Lang>
{
    fn from(tokens: &'slice [RichToken<'src, Lang>]) -> Self {
        Self {
            src_pos: 0,
            trivia_tokens: &[],
            trivia_tokens_text_len: 0,
            tokens,
        }
        .advance_trivia()
    }
}

impl<'slice, 'src, Lang: Language> nom::InputLength for Input<'slice, 'src, Lang> {
    fn input_len(&self) -> usize {
        self.trivia_tokens.len() + self.tokens.len()
    }
}

pub struct Children<Lang: Language, E> {
    errors: Vec<E>,
    inner: Vec<NodeOrToken<GreenNode, GreenToken>>,
    _lang: PhantomData<Lang>,
}

impl<Lang: Language, E> Default for Children<Lang, E> {
    fn default() -> Self {
        Self {
            errors: vec![],
            inner: vec![],
            _lang: PhantomData,
        }
    }
}

impl<Lang: Language, E> Children<Lang, E> {
    fn empty() -> Self {
        Self::default()
    }

    fn from_tokens<'src, I: IntoIterator<Item = (Lang::Kind, &'src str)>>(iter: I) -> Self {
        Self {
            inner: iter
                .into_iter()
                .map(|(token, str)| {
                    NodeOrToken::Token(GreenToken::new(Lang::kind_to_raw(token), str))
                })
                .collect(),
            ..Self::default()
        }
    }

    fn from_err(error: E) -> Self {
        Self {
            errors: vec![error],
            // TODO include inner non-matched nodes/tokens ?
            inner: vec![NodeOrToken::Node(GreenNode::new(
                Lang::kind_to_raw(Lang::get_error_kind()),
                [],
            ))],
            ..Self::default()
        }
    }

    fn from_rowan_children(children: rowan::Children, errors: Vec<E>) -> Self {
        Self {
            errors,
            inner: children
                .map(|e| match e {
                    NodeOrToken::Token(t) => NodeOrToken::Token(t.to_owned()),
                    NodeOrToken::Node(n) => NodeOrToken::Node(n.to_owned()),
                })
                .collect(),
            ..Self::default()
        }
    }

    fn add(&mut self, other: Self) {
        self.errors.extend(other.errors);
        self.inner.extend(other.inner);
    }

    fn into_node(self, kind: Lang::Kind) -> Self {
        Self {
            errors: self.errors,
            inner: vec![NodeOrToken::Node(GreenNode::new(
                Lang::kind_to_raw(kind),
                self.inner,
            ))],
            ..Self::default()
        }
    }
}

pub type IResult<'slice, 'src, Lang, E, IE = E> =
    nom::IResult<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>;
pub type RootIResult<'slice, 'src, Lang, E, IE = E> =
    nom::IResult<Input<'slice, 'src, Lang>, (SyntaxNode<Lang>, Vec<E>), IE>;

/// Parses only the given token, fails if the wrong token is read or if the input is empty
///
/// Trivia tokens are automatically skipped and prepended to the parsed token
pub fn t<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    token: Lang::Kind,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> {
    debug_assert!(
        !Lang::is_trivia(token),
        "this parser will always fail with trivia tokens, please use `t_raw`"
    );

    move |input| {
        if let Some((current_token, current_token_str)) = input.tokens.first() {
            if *current_token == token {
                let trivia = input.trivia_tokens;
                Ok((
                    input.next(),
                    Children::from_tokens(
                        trivia
                            .iter()
                            .cloned()
                            .chain(std::iter::once((*current_token, *current_token_str))),
                    ),
                ))
            } else {
                Err(nom::Err::Error(IE::from_message("unexpected token")))
            }
        } else {
            Err(nom::Err::Error(IE::from_unexpected_eof(input.src_pos)))
        }
    }
}

/// Parses only the given token, fails if the wrong token is read or if the input is empty, **but
/// does not skip trivia tokens**
///
/// This may be useful in situations were whitespace or comments are prohibited in a specific
/// context
pub fn t_raw<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    token: Lang::Kind,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> {
    let is_trivia = Lang::is_trivia(token);

    move |input| {
        let input_src_pos = input.src_pos;

        if is_trivia {
            if let Some((new_input, current_token)) = input.next_trivia() {
                if current_token.0 == token {
                    Ok((
                        new_input,
                        Children::from_tokens(std::iter::once(*current_token)),
                    ))
                } else {
                    Err(nom::Err::Error(IE::from_message("unexpected token")))
                }
            } else {
                Err(nom::Err::Error(IE::from_unexpected_eof(input_src_pos)))
            }
        } else {
            t(token)(input)
        }
    }
}

pub fn fallible_with<'slice, 'src: 'slice, Lang: Language, E, IE>(
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
    mut convert: impl FnMut(IE) -> E,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        let conv = &mut convert;
        parser.parse(input.clone()).or_else(move |e| match e {
            nom::Err::Error(e) => Ok((input, Children::from_err(conv(e)))),
            other => Err(other),
        })
    }
}

pub fn fallible<'slice, 'src: 'slice, Lang: Language, E, IE>(
    parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
    E: From<IE>,
{
    fallible_with(parser, Into::into)
}

/// Wraps the contained parser's direct and indirect output into a node
pub fn node<'slice, 'src: 'slice, Lang: Language, E, IE>(
    node: Lang::Kind,
    parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnOnce(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| map(parser, |c| c.into_node(node))(input)
}

/// Wraps the contained parser's output into a root node inside a [SyntaxNode]
pub fn root_node<'slice, 'src: 'slice, Lang: Language, E, IE>(
    node: Lang::Kind,
    parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnOnce(Input<'slice, 'src, Lang>) -> RootIResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    let syntax = Lang::kind_to_raw(node);
    move |input| {
        map(parser, |c| {
            (
                SyntaxNode::new_root(GreenNode::new(syntax, c.inner)),
                c.errors,
            )
        })(input)
    }
}

pub trait Joignable<'slice, 'src, Lang: Language, E, IE> {
    fn parse(&mut self, input: Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>;
}

macro_rules! impl_tuple {
    ($arg0:ident, $($arg:ident),*) => {
        #[allow(unused_parens)]
        impl<'slice, 'src: 'slice, Lang: Language, CE, IE, $arg0, $($arg),*> Joignable<'slice, 'src, Lang, CE, IE> for ($arg0, $($arg,)*)
        where
            Lang::Kind: 'static,
            $arg0: Parser<Input<'slice, 'src, Lang>, Children<Lang, CE>, IE>,
            $($arg: Parser<Input<'slice, 'src, Lang>, Children<Lang, CE>, IE>),*
        {
            fn parse(&mut self, input: Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, CE, IE> {
                #[allow(non_snake_case)]
                let ($arg0, $($arg),*) = self;

                #[allow(unused_mut)]
                let (input, mut children) = $arg0.parse(input)?;
                $(let (input, children2) = $arg.parse(input)?; children.add(children2);)*

                Ok((input, children))
            }
        }
    };
}

impl_tuple!(A,);
impl_tuple!(A, B);
impl_tuple!(A, B, C);
impl_tuple!(A, B, C, D);
impl_tuple!(A, B, C, D, E);
impl_tuple!(A, B, C, D, E, F);
impl_tuple!(A, B, C, D, E, F, G);
impl_tuple!(A, B, C, D, E, F, G, H);
impl_tuple!(A, B, C, D, E, F, G, H, I);
impl_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);

/// Joins the output trees of multiple parsers without creating a new node (yet)
pub fn join<'slice, 'src: 'slice, Lang: Language, E, IE>(
    mut parsers: impl Joignable<'slice, 'src, Lang, E, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> {
    move |input| parsers.parse(input)
}

/// Repeats the given parser 0 or more times until it fails, and join all the results in a
/// `Children` object
pub fn many0<'slice, 'src: 'slice, E, Lang: Language, IE>(
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, E>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        let (mut input, mut first) = match parser.parse(input.clone()) {
            Ok(x) => x,
            Err(_) => return Ok((input, Children::empty())),
        };

        while let Ok((new_input, new_children)) = parser.parse(input.clone()) {
            input = new_input;
            first.add(new_children);
        }

        Ok((input, first))
    }
}

/// FIXME: remove all the following once the nom migration is complete
pub mod adapter {
    use super::*;
    use crate::parser::Parser;
    use rowan::GreenNodeBuilder;

    const DUMMY_SYNTAX: SyntaxKind = SyntaxKind(65535);

    pub fn old_style<'slice, 'src: 'slice, IE: RowanNomError<crate::LustreLang>>(
        f: impl Fn(&mut Parser<'src>) -> bool,
    ) -> impl FnMut(
        Input<'slice, 'src, crate::LustreLang>,
    ) -> IResult<'slice, 'src, crate::LustreLang, crate::Error, IE> {
        move |input| {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(DUMMY_SYNTAX);

            let mut parser = Parser {
                tokens: input
                    .trivia_tokens
                    .iter()
                    .chain(input.tokens)
                    .rev()
                    .cloned()
                    .collect(),
                builder,
                errors: vec![],
                pos: input.src_pos,
            };

            if f(&mut parser) {
                parser.builder.finish_node();

                let consumed = input.tokens.len() + input.trivia_tokens.len() - parser.tokens.len();
                let mut input2 = input;
                for _ in 0..consumed {
                    input2.next_raw();
                }

                Ok((
                    input2,
                    Children::from_rowan_children(
                        parser.builder.finish().children(),
                        parser.errors,
                    ),
                ))
            } else {
                Err(nom::Err::Error(IE::from_message("parser failed")))
            }
        }
    }
}
