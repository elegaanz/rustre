# Architecture des modules

## Diagramme des modules prévisionnel

Ce diagramme n'inclut que les principaux modules.

```plantuml
digraph a {

// Bibliothèques

"rowan", "nom", "salsa", "clap" [shape=rect]
"rowan" [href="https://lib.rs/crates/rowan"]
"nom" [href="https://lib.rs/crates/nom"]
"salsa" [href="https://lib.rs/crates/salsa"]
"clap" [href="https://lib.rs/crates/clap"]

// Libs internes

"rustre-parser" -> "rowan-nom" -> "rowan", "nom"
"rustre-parser" -> "rowan", "nom", "rustre-parser-tests-codegen"
"rustre-core" -> "rustre-parser", "salsa"
"rustre-cli" -> "rustre-core", "clap"

}
```

> **Légende:**
>
> `⬭` Modules internes (développés par nous) <br/>
> `□` Modules externes (dépendances sur des bibliothèques développées à part)

## Responsabilité des modules

  * `rustre-parser-tests-codegen`: Génère des tests unitaires pour `rustre-parser` à partir des fichiers de tests du dépôt Lustre officiel
  * `rustre-parser`: Parseur et lexeur Lustre, production d'AST rowan
  * `rustre-core`: Salsa, type-checking, ...
  * `rustre-cli`: Point d'entrée de l'interface en ligne de commande

## Dépendances externes (bibliothèques)

Les dépendances externes sont distribuées grâce au gestionnaire de paquets officiel du langage Rust, `cargo`, et hébergées sur <https://crates.io>. Nous conseillons le site non-officiel <https://lib.rs> comme front-end plus joli à _crates.io_. Les dépendances listées ci-dessus sont non-exhaustives; certaines n'ont pas d'intérêt à être détaillées et des dizaines/centaines d'autres sont <abbr title="dépendances d'une dépendance (indirectement dépendantes)">transitives</abbr>.

L'intérêt des principales bibliothèques est rapidement passé en revue dans [le glossaire](../annexes/glossary.md).
