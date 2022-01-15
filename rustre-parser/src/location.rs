#[derive(Default, PartialEq, Clone, Debug)]
pub struct Location {
    pub line: u64,
    pub col: u64,
    pub pos: u64,
}

#[derive(Default, PartialEq, Clone)]
pub struct Span<'f> {
    pub file: &'f str,
    pub start: Location,
    pub end: Location,
}

impl<'f> std::fmt::Debug for Span<'f> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "'{}' {}:{}..{}:{}",
            self.file, self.start.line, self.start.col, self.end.line, self.end.col
        ))
    }
}

#[derive(PartialEq)]
pub struct Spanned<'f, T> {
    pub span: Span<'f>,
    pub item: T,
}

impl<'f, T: std::fmt::Debug> std::fmt::Debug for Spanned<'f, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:#?} \x1b[38;5;240m@ {:?}\x1b[0m",
            self.item, self.span
        ))
    }
}

impl<'f, T: Clone> Clone for Spanned<'f, T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span.clone(),
            item: self.item.clone(),
        }
    }
}

impl<'f, T> From<Spanned<'f, T>> for Span<'f> {
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

impl<'f, T> Spanned<'f, T> {
    pub fn boxed(self) -> Spanned<'f, Box<T>> {
        self.map(Box::new)
    }

    pub fn map_ref<F, U>(&self, f: F) -> Spanned<'f, U>
    where
        F: Fn(&T) -> U,
    {
        Spanned {
            span: self.span.clone(),
            item: f(&self.item),
        }
    }

    pub fn map<F, U>(self, f: F) -> Spanned<'f, U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            span: self.span,
            item: f(self.item),
        }
    }
}
