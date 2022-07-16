use std::ops::Range;

pub trait RowanNomError<Lang: super::Language> {
    /// Generic error, should probably be ultimately removed
    fn from_message(message: &str) -> Self;

    /// Creates error: Attempted to read a token when there are none remaining
    fn from_unexpected_eof(position: usize) -> Self;

    fn from_unexpected_token(span: Range<usize>, expected: Lang::Kind, found: Lang::Kind) -> Self;

    // TODO add much more errors, include location information in constructors
}
