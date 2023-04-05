use ariadne::{Color, FnCache, Label, Report, ReportKind};
use rustre_core::diagnostics::{Diagnostic, Level};
use std::fmt::{Debug, Display, Formatter};
use std::path::Path;
use yeter::Database;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Path2<'a>(&'a Path);

impl<'a> Display for Path2<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0.display(), f)
    }
}

pub fn print_diagnostics(db: &Database, deny_warnings: bool) -> Result<(), u8> {
    rustre_core::check(db);

    let effects = db.effect::<Diagnostic>();

    let mut errors = 0usize;
    let mut warnings = 0usize;
    for diagnostic in effects {
        let kind = match diagnostic.level {
            Level::Debug => ReportKind::Custom("Debug", Color::Blue),
            Level::Info => ReportKind::Advice,
            Level::Warning => {
                warnings += 1;
                ReportKind::Warning
            }
            Level::Error => {
                errors += 1;
                ReportKind::Error
            }
        };

        let cache = FnCache::new(|path: &Path2| {
            std::fs::read_to_string(path.0).map_err(|b| Box::new(b) as Box<dyn Debug>)
        });

        let (path, offset) = diagnostic.file_context().unwrap();
        let mut report = Report::build(kind, Path2(path), offset).with_message(&diagnostic.message);

        for (idx, (span, message)) in diagnostic.attachments.iter().enumerate() {
            let span = (Path2(span.file.as_path()), span.start..span.end);
            report = report.with_label(
                Label::new(span)
                    .with_message(message)
                    .with_order(idx as i32),
            );
        }

        report.finish().eprint(cache).unwrap();
    }

    if errors > 0 || (deny_warnings && warnings > 0) {
        Err(1)
    } else {
        Ok(())
    }
}
