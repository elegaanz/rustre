use std::ops::Range;

pub trait RowanNomError<Lang: super::Language> {
    /// Generic error, should probably be ultimately removed
    fn from_message(message: &str) -> Self;

    /// Creates error: Attempted to read a token when there are none remaining
    fn from_unexpected_eof(position: usize) -> Self;

    fn from_unexpected_token(span: Range<usize>, expected: Lang::Kind, found: Lang::Kind) -> Self;

    fn with_context(self, ctx: &'static str) -> Self;

    // TODO add much more errors, include location information in constructors
}

/// A ZST that implements [`RowanNomError`] when details or context don't matter
#[derive(Debug, Default, Clone, Copy, thiserror::Error)]
#[error("dummy error used, can't provide further information")]
pub struct DummyError;

impl<Lang: super::Language> RowanNomError<Lang> for DummyError {
    fn from_message(_message: &str) -> Self {
        Self
    }

    fn from_unexpected_eof(_position: usize) -> Self {
        Self
    }

    fn from_unexpected_token(
        _span: Range<usize>,
        _expected: Lang::Kind,
        _found: Lang::Kind,
    ) -> Self {
        Self
    }

    fn with_context(self, _ctx: &'static str) -> Self {
        Self
    }
}
