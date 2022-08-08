mod children;
mod error;

pub use nom::combinator::map;

pub use children::Children;
pub use error::*;
use nom::Parser;
use rowan::SyntaxNode;

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
    pub fn src_pos(&self) -> usize {
        self.src_pos
    }

    fn next(
        &self,
    ) -> Option<(
        Self,
        (
            &'slice [RichToken<'src, Lang>],
            &'slice RichToken<'src, Lang>,
        ),
    )> {
        match self.tokens {
            [] => None,
            [first, rest @ ..] => {
                let relevant_tokens = (self.trivia_tokens, first);

                let input = Input {
                    src_pos: self.src_pos + self.trivia_tokens_text_len + first.1.len(),
                    trivia_tokens: &[],
                    trivia_tokens_text_len: 0,
                    tokens: rest,
                }
                .advance_trivia();

                Some((input, relevant_tokens))
            }
        }
    }

    fn next_trivia(&self) -> Option<(Self, &'slice RichToken<'src, Lang>)> {
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

pub type IResult<'slice, 'src, Lang, E, IE = E> =
    nom::IResult<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>;
pub type RootIResult<'slice, 'src, Lang, E, IE = E> =
    nom::IResult<Input<'slice, 'src, Lang>, (SyntaxNode<Lang>, Vec<E>), IE>;

/// Succeeds without consuming the input
///
/// May be used as an [alt] case
pub fn success<'slice, 'src: 'slice, Lang: Language, E, IE>(
    input: Input<'slice, 'src, Lang>,
) -> IResult<'slice, 'src, Lang, E, IE> {
    Ok((input, Children::empty()))
}

/// Succeeds if the input contains no tokens other than trivia, and returns a [Children] object with
/// said trivia, effectively emptying last insignificant bits of input
pub fn eof<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    input: Input<'slice, 'src, Lang>,
) -> IResult<'slice, 'src, Lang, E, IE> {
    if input.tokens.is_empty() {
        let trivia_tokens = input.trivia_tokens.into_iter();

        let input = Input {
            src_pos: input.src_pos + input.trivia_tokens_text_len,
            trivia_tokens: &[],
            trivia_tokens_text_len: 0,
            tokens: &[],
        };

        Ok((input, trivia_tokens.collect()))
    } else {
        Err(nom::Err::Error(IE::from_message(
            "expected eof, found token",
        )))
    }
}

pub fn t_any<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    input: Input<'slice, 'src, Lang>,
) -> IResult<'slice, 'src, Lang, E, IE> {
    if let Some((new_input, (trivia, current_token))) = input.next() {
        Ok((
            new_input,
            trivia
                .iter()
                .chain(std::iter::once(current_token))
                .collect(),
        ))
    } else {
        Err(nom::Err::Error(IE::from_unexpected_eof(input.src_pos)))
    }
}

/// Parses only the given token, fails if the wrong token is read or if the input is empty
///
/// Trivia tokens are automatically skipped and prepended to the parsed token
pub fn t<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    token: Lang::Kind,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> + Clone + Copy {
    debug_assert!(
        !Lang::is_trivia(token),
        "this parser will always fail with trivia tokens, please use `t_raw`"
    );

    move |input| {
        if let Some((new_input, (trivia, current_token))) = input.next() {
            if current_token.0 == token {
                Ok((
                    new_input,
                    trivia
                        .iter()
                        .chain(std::iter::once(current_token))
                        .collect(),
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
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> + Clone + Copy {
    let is_trivia = Lang::is_trivia(token);

    move |input| {
        let input_src_pos = input.src_pos;

        if is_trivia {
            if let Some((new_input, current_token)) = input.next_trivia() {
                if current_token.0 == token {
                    Ok((new_input, std::iter::once(current_token).collect()))
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

/// Makes a parser peek without consuming
///
/// Returns `Ok((same_input, Children::empty()))` or the error returned by the wrapped parser
///
/// Works the same as [`nom::combinator::peek`]
pub fn peek<'slice, 'src: 'slice, Lang: Language, E, IE>(
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        let (_, _) = parser.parse(input.clone())?;
        Ok((input, Children::empty()))
    }
}

/// Negated peek, succeeds only if the given parser fails
///
///   * On failure, succeeds with and empty [`Children`] and the same input
///   * On success, fails
pub fn peek_neg<'slice, 'src: 'slice, Lang: Language, E, IE: RowanNomError<Lang>>(
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| match parser.parse(input.clone()) {
        Ok(_) => Err(nom::Err::Error(IE::from_message("neg_peek"))),
        Err(_) => Ok((input, Children::empty())),
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

pub fn expect<'slice, 'src: 'slice, Lang: Language, E: RowanNomError<Lang>>(
    parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, E>,
    message: &'static str,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, E>
where
    Lang::Kind: 'static,
{
    fallible_with(parser, move |e| e.with_context(message))
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
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        parser
            .parse(input)
            .map(|(input, c)| (input, c.into_node(node)))
    }
}

/// Wraps the contained parser's output into a root node inside a [SyntaxNode]
pub fn root_node<'slice, 'src: 'slice, Lang: Language, E, IE>(
    node: Lang::Kind,
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> RootIResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        parser
            .parse(input)
            .map(|(input, c)| (input, c.into_root_node(node)))
    }
}

pub trait Joignable<'slice, 'src, Lang: Language, E, IE> {
    fn parse(&mut self, input: Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>;
}

macro_rules! impl_joignable {
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
                $(let (input, children2) = $arg.parse(input)?; children += children2;)*

                Ok((input, children))
            }
        }
    };
}

impl_joignable!(A,);
impl_joignable!(A, B);
impl_joignable!(A, B, C);
impl_joignable!(A, B, C, D);
impl_joignable!(A, B, C, D, E);
impl_joignable!(A, B, C, D, E, F);
impl_joignable!(A, B, C, D, E, F, G);
impl_joignable!(A, B, C, D, E, F, G, H);
impl_joignable!(A, B, C, D, E, F, G, H, I);
impl_joignable!(A, B, C, D, E, F, G, H, I, J);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y);
impl_joignable!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

/// Joins the output trees of multiple parsers without creating a new node (yet)
pub fn join<'slice, 'src: 'slice, Lang: Language, E, IE>(
    mut parsers: impl Joignable<'slice, 'src, Lang, E, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE> {
    move |input| parsers.parse(input)
}

pub fn opt<'slice, 'src: 'slice, Lang: Language, E, IE>(
    mut parser: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| match parser.parse(input.clone()) {
        Ok(ok) => Ok(ok),
        Err(nom::Err::Error(_)) => Ok((input, Children::empty())),
        Err(e) => Err(e),
    }
}

pub trait Alt<I, O, E> {
    fn parse(&mut self, input: I) -> nom::IResult<I, O, E>;
}

macro_rules! impl_alt {
    ($arg0:ident, $($arg:ident),*) => {
        #[allow(unused_parens)]
        impl<II: Clone, OO, EE, $arg0, $($arg),*> Alt<II, OO, EE> for ($arg0, $($arg,)*)
        where
            $arg0: Parser<II, OO, EE>,
            $($arg: Parser<II, OO, EE>),*
        {
            fn parse(&mut self, input: II) -> nom::IResult<II, OO, EE> {
                #[allow(non_snake_case)]
                let ($arg0, $($arg),*) = self;

                $arg0.parse(input.clone())
                $(.or_else(|_| $arg.parse(input.clone())))*
            }
        }
    };
}

impl_alt!(A,);
impl_alt!(A, B);
impl_alt!(A, B, C);
impl_alt!(A, B, C, D);
impl_alt!(A, B, C, D, E);
impl_alt!(A, B, C, D, E, F);
impl_alt!(A, B, C, D, E, F, G);
impl_alt!(A, B, C, D, E, F, G, H);
impl_alt!(A, B, C, D, E, F, G, H, I);
impl_alt!(A, B, C, D, E, F, G, H, I, J);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y);
impl_alt!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

pub fn alt<I, O, E>(mut parsers: impl Alt<I, O, E>) -> impl FnMut(I) -> nom::IResult<I, O, E> {
    move |input| parsers.parse(input)
}

/// Repeats the given parser 0 or more times until it fails, and join all the results in a
/// `Children` object
pub fn many0<'slice, 'src: 'slice, Lang: Language, E, IE>(
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
            first += new_children;
        }

        Ok((input, first))
    }
}

/// Similar to [`nom::multi::fold_many1`], but more specialized
///
/// Useful for parsing left-associative expressions
pub fn fold_many1<'slice, 'src: 'slice, Lang: Language, C, E, IE>(
    mut init: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
    mut then: impl Parser<Input<'slice, 'src, Lang>, C, IE>,
    mut merge: impl FnMut(Children<Lang, E>, C) -> Children<Lang, E>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |input| {
        let (mut input, mut children) = init.parse(input)?;

        while let Ok((new_input, new_children)) = then.parse(input.clone()) {
            input = new_input;
            children = merge(children, new_children);
        }

        Ok((input, children))
    }
}

/// Similar to [`fold_many1`], but folds right instead of left
///
/// Useful for parsing right-associative expressions
pub fn fold_many1_right<'slice, 'src: 'slice, Lang: Language, C, E, IE: RowanNomError<Lang>>(
    mut cont: impl Parser<Input<'slice, 'src, Lang>, C, IE>,
    mut end: impl Parser<Input<'slice, 'src, Lang>, Children<Lang, E>, IE>,
    mut merge: impl FnMut(Children<Lang, E>, C) -> Children<Lang, E>,
) -> impl FnMut(Input<'slice, 'src, Lang>) -> IResult<'slice, 'src, Lang, E, IE>
where
    Lang::Kind: 'static,
{
    move |mut input| {
        let mut stack = Vec::new();

        loop {
            if let Ok((new_input, new_children)) = cont.parse(input.clone()) {
                input = new_input;
                stack.push(new_children);
            } else if let Ok((input, new_children)) = end.parse(input) {
                let children = stack.into_iter().rfold(new_children, |a, b| merge(a, b));
                break Ok((input, children));
            } else {
                break Err(nom::Err::Error(IE::from_message("couldn't finish fold")));
            }
        }
    }
}
