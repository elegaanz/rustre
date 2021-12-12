#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug)]
pub struct Location {
    pub line: u64,
    pub col: u64,
    pub pos: u64,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug)]
pub struct Span<'f> {
    pub file: &'f str,
    pub start: Location,
    pub end: Location,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct Spanned<'f, T: std::fmt::Debug> {
    pub span: Span<'f>,
    pub item: T,
}

impl<'f, T: std::fmt::Debug + Clone> Clone for Spanned<'f, T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span.clone(),
            item: self.item.clone(),
        }
    }
}

impl<'f, T: std::fmt::Debug> From<Spanned<'f, T>> for Span<'f> {
    fn from(spanned: Spanned<'f, T>) -> Span<'f> {
        spanned.span
    }
}

impl<'a, 'f, T: std::fmt::Debug> From<&'a Spanned<'f, T>> for Span<'f> {
    fn from(spanned: &'a Spanned<'f, T>) -> Span<'f> {
        spanned.span.clone()
    }
}

impl<'f, T: std::fmt::Debug> Spanned<'f, T> {
    pub fn fusion(start: impl Into<Span<'f>>, end: impl Into<Span<'f>>, item: T) -> Self {
        let start = start.into();
        let end = end.into();
        let span = Span {
            file: start.file, // TODO: panic if start.file != end.file ?
            start: start.start,
            end: end.end,
        };
        Self { span, item }
    }
}
