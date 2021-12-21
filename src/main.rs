use ariadne::{Label, Report, ReportKind};

mod lexer;
mod location;
mod parser;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(&file).unwrap();
    let mut lexer = lexer::Lexer::new(&file, &contents);
    let lex = lexer.lex().unwrap();
    let mut parser = parser::Parser::new();
    let ast = parser.parse(&lex);
    match ast {
        Ok(ast) => {
            if !parser.errors.is_empty() {
                for err in parser.errors {
                    print_err(err, &contents);
                }
                println!("Partial AST was built :");
            } else {
                println!("Parsing: OK");
            }
            println!("{:#?}", ast)
        }
        Err(e) => print_err(e, &contents),
    }
}

fn print_err<'a, 'f>(err: parser::Error<'a, 'f>, contents: &'a str) {
    match err {
        parser::Error::UnexpectedToken(toks, exp) => {
            let span = toks
                .get(0)
                .map(|t| t.span.clone())
                .unwrap_or(location::Span {
                    file: "??",
                    start: location::Location {
                        line: 0,
                        col: 0,
                        pos: 0,
                    },
                    end: location::Location {
                        line: 0,
                        col: 0,
                        pos: 0,
                    },
                });
            let path = span.file;
            let start = span.start.pos as usize;
            let end = span.end.pos as usize;
            Report::build(ReportKind::Error, path.to_owned(), start)
                .with_message(format!(
                    "Unexpected token {}",
                    toks.get(0)
                        .map(|t| format!("{:?}", t.item))
                        .unwrap_or_default(),
                ))
                .with_label(Label::new((path.to_owned(), start..end)).with_message(exp))
                .finish()
                .print(ariadne::sources(vec![(path.to_owned(), contents)]))
                .unwrap();
        }
        parser::Error::ReportedError => {}
    }
}
