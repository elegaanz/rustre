# Lexeur (logos)

[Code source]

Pour écrire un lexeur facilement, on utilise la crate [`logos`].

Cette crate fournit une macro procédurale qu'on peut appliquer à un enumération.
Chaque variante de l'énumération est un token possible, et on les annote avec
un attribut indiquant comment reconnaître ce token.

```rust
use logos::Logos; // on importe la macro

#[derive(Logos)] // on l'utilise pour générer le code du lexeur
enum Token {
    #[token("let")]
    Let,

    #[token("function")]
    Function,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    Space,

    // etc.
}

fn main() {
    // la fonction Token::lexer est générée par logos
    let mut lexer = Token::lexer("let let function let");
    // lexer est un itérateur, donc on peut récupérer la liste de tous les
    // tokens avec un collect
    let tokens: Vec<_> = lexer.collect();
    assert_eq!(
        tokens,
        vec![ Token::Let, Token::Let, Token::Function, Token::Let ],
    );
}
```

Pour les différents attributs disponibles, voir la [documentation de logos].

Normalement le lexeur est complet, mais si besoin d'autres tokens pourront être ajoutés.

Attention, beaucoup de variantes de l'énumaration `Token` ne sont en fait pas des tokens
qu'on peut retrouver dans le code source, mais des étiquettes pour l'arbre de syntaxe
(cf. le chapitre suivant). Seuls ceux avec un attribut de logos sont de vrais tokens.

## Limitations

TODO

[Code source]: @@@rustre-parser/src/lexer.rs
[`logos`]: https://docs.rs/logos/latest/logos/
[documentation de logos]: https://docs.rs/logos/latest/logos/derive.Logos.html