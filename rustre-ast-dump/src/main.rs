use std::ops::Range;

use ariadne::{Label, Report, ReportKind, Span};
use rowan::NodeOrToken;
use rustre_parser::{lexer::Token, Parse, SyntaxNode, SyntaxToken};

fn main() {
    let file = std::env::args().nth(1).expect("please give a file name");
    let contents = std::fs::read_to_string(&file).unwrap();
    let Parse { root, errors } = rustre_parser::Parse::parse(&contents);
    print(0, root.into());
    for err in errors {
        Report::<(String, Range<usize>)>::build(ReportKind::Error, file.clone(), err.span.start())
            .with_message(err.msg)
            .with_label(Label::new((file.clone(), err.span)))
            .finish()
            .print(ariadne::sources(vec![(file.clone(), &contents)]))
            .unwrap();
    }
}

fn print(indent: usize, element: rowan::NodeOrToken<SyntaxNode, SyntaxToken>) {
    let kind: Token = element.kind().into();
    print!("{:indent$}", "", indent = indent);
    match element {
        rowan::NodeOrToken::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}
