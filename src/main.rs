mod lexer;
mod location;
mod parser;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(&file).unwrap();
    let mut lexer = lexer::Lexer::new(&file, &contents);
    let lex = lexer.lex().unwrap();
    let ast = parser::Parser::parse(&lex);
    println!("{:#?}", ast);
}
