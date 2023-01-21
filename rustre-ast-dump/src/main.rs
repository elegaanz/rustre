use std::ops::Range;

use ariadne::{Label, Report, ReportKind, Span};
use rowan::NodeOrToken;
use rustre_parser::ast::AstNode;
use rustre_parser::{lexer::Token, SyntaxNode, SyntaxToken};

fn main() -> Result<(), u8> {
    let file = std::env::args().nth(1).expect("please give a file name");
    let mut contents = std::fs::read_to_string(&file).unwrap();

    // Remove when comments as last tokens are properly parsed
    if !contents.ends_with('\n') {
        contents += "\n";
    }

    let (root, errors) = rustre_parser::parse(&contents);
    print(0, NodeOrToken::Node(root.syntax().clone()));

    let no_errors = errors.is_empty();

    for err in errors {
        Report::<(String, Range<usize>)>::build(ReportKind::Error, file.clone(), err.span.start())
            .with_message(err.msg)
            .with_label(Label::new((file.clone(), err.span)))
            .finish()
            .print(ariadne::sources(vec![(file.clone(), &contents)]))
            .unwrap();
    }

    if no_errors {
        Ok(())
    } else {
        Err(1)
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
