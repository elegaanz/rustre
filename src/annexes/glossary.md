# Glossaire

Ce lexique définit des termes spécifiques à notre projet. Les définitions ne sont pas objectives et peuvent légèrement varier des définitions communément admises.

  * **Analyseur lexical:** Algorithme qui scinde un texte en _lexèmes localisés_. Situé dans `rustre-parser`.
  * **Arbre de syntaxe sans perte:** Arbre de syntaxe qui inclut tous les _lexèmes_ passés en entrée, y compris les espaces, saut de lignes, commentaires et _lexèmes_ non-reconnus (sous un lexème `Error` spécial). On peut en restituer le fichier d'entrée sans aucune perte d'informations.
  * **Arbre de syntaxe typé:** Arbre de syntaxe abstraite qui représente la structure du programme sans informations superflues (espaces blancs, commentaires) et où les valeurs sont typées. On se laisse la possibilité de conserver les commentaires "de documentation" dans l'AST dans l'éventualité de construire un générateur de documentation similaire à `rustdoc`, `javadoc` ou `doxygen`.
  * **[`clap`](https://lib.rs/crates/clap) _(command-line argument parser)_:** _Bibliothèque_ permettant de parser les arguments de ligne de commande
  * **[Lexème](https://fr.wikipedia.org/wiki/Analyse_lexicale#Lex%C3%A8me):** Elément atomique produit par un _analyseur lexical_. Une différence par rapport à beaucoup d'implémentations est qu'il existe dans notre cas des lexèmes spéciaux pour représenter les espaces blancs, les commentaires, les sauts de ligne et même les lexèmes non reconnus (`Error`).
  * **Lexème localisé:** Lexème accompagné d'un intervalle d'entiers correspondant à sa position dans le texte. Contrairement à d'autres représentations possibles de lexèmes, les nôtres ne contiennent pas leur valeur lorsqu'ils sont paramétriques. Par exemple, un lexème correspondant à un nombre entier litéral s'appelle juste `IConst` et ne stocke pas la valeur de l'entier. Si on veut la récupérer, il faut utiliser la localisation du lexème et parser ce nombre dans le code source a posteriori.
  * **[`logos`](https://lib.rs/crates/logos):** _Bibliothèque_ pour générer un _analyseur lexical_ à partir d'un `enum` Rust dont chaque <abbr title="option d'un enum">variante</abbr> est annotée. Par exemple, pour lexer des expressions mathématiques simples, on pourrait définir:
    ```rust,noplayground
    #[derive(Logos)]
    enum Token {
      #[token("+")]
      Plus,

      #[token("-")]
      Minus,

      #[regex(r"\d+")]
      Literal,

      #[regex(r"\s+")]
      Whitespace,

      #[error]
      Error,
    }
    ```
    ...et `logos` s'occupe de nous générer la fonction d'analyse lexicale.
  * **[`nom`](https://lib.rs/crates/nom):** _Bibliothèque_ pour construire des parseurs basés sur des [_combinateurs_](https://en.wikipedia.org/wiki/Parser_combinator)
  * **[`rowan`](https://lib.rs/crates/rowan):** _Bibliothèque_ pour représenter et construire des _arbres de syntaxe génériques sans perte_
  * **[`salsa`](https://lib.rs/crates/salsa):** _Bibliothèque_ pour concevoir un système de compilation incrémentale basée sur des "queries" séparées en 2 types: des "fonctions" (pures, typées, et mises en cache sur disque), et des "inputs" (généralement des fichiers sources)
