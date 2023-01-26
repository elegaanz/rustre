# Parsing résilient

## Problématique

Le parsing correspond à la traduction d'une séquence ordonnée de lexèmes en un arbre syntaxique plus structuré. L'approche traditionelle consiste à définir l'arbre de syntaxe _en dur_, grâce à des structures. Par exemple:

```rust,noplayground
// ...

enum Statement {
    VoidExpression(Expression), // expression au résultat non utilisé, appel de fonction
    VariableDeclaration {
        name: String,
        explicit_type: Option<Type>,
        initializer: Option<Expression>,
    },
    If {
        condition: Expression,
        then: Block,
        r#else: Block,
    },
    Return(Expression),
    // ...
}

// ...
```

Pour construire une telle structure, de nombreuses approches existent déjà (LL, LR, LALR) et peuvent être implémentées de différentes manières (combinateurs, générateurs Yacc/Bison/JavaCC/etc.). Un inconvénient majeur réside dans la structure de données elle même, qui n'est pas capable de représenter d'éventuelles erreurs. Concrètement, si un lexème non attendu est lu à un moment donné, le parseur s'arrête, renvoie une erreur, et ne pourra pas "retomber sur ses pieds". On se retrouve alors avec un message du style:

```
Error: in file "~/fichier_avec_erreur.lus", line 11, col 12 to 12, token ';':
Error: Syntax error


bye
```
<small>Sympa non? Cet exemple est produit par lv6, le compilateur Lustre officiel.</small>

Ces échecs ne sont pas pratiques pour de nombreuses raisons:
  * Si un fichier est plein d'erreurs, on ne pourra résoudre les erreurs qu'une par une, du début à la fin, en relançant la compilation à chaque fois qu'on pense en avoir réglée une. Il nous sera impossible d'avoir un message d'erreur sur une erreur à la fin du fichier tant que tout le début du fichier n'est pas correctement parsé.
  * Il est impossible de faire un [_language server_](https://en.wikipedia.org/wiki/Language_Server_Protocol) utilisable, pour intégrer le langage dans un IDE. Au cours de l'écriture d'un code, il y a probablement plus d'instants où le code n'est pas gramaticalement correct que l'inverse. Si notre analyse syntaxique échoue systématiquement tant que le code n'est pas parfait, notre serveur de langage a un intérêt très limité, et certaines fonctionnalités comme l'autocomplétion n'ont aucun sens à être implémentées.

## Rendre l'arbre de syntaxe moins rigide

Intrinsèquement, **notre problème ne vient pas du type de parseur utilisé**, **mais bien de la _représentation_ de l'AST** sous la forme d'une structure avec des champs biens définis, qui se doivent d'être présents. On souhaiterait au contraire permettre à des éléments d'un noeud syntaxique de ne pas être présents ou d'être au mauvais endroit, tout en structurant quand même la suite de lexèmes sous la forme d'un arbre.

Plutôt que de réinventer la roue, nous utiliseront les structures de données fournies par la bibliothèque Rust [`rowan`](https://lib.rs/crates/rowan), qui est inspiré par la [`libsyntax`](https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax) de Swift et est développée par l'équipe derrière le _language server_ officiel Rust, [`rust-analyzer`](https://rust-analyzer.github.io/).

Il y a plusieurs concepts intéressants qu'impose/incite l'usage de cette bibliothèque:
  * Les arbres de syntaxe traditionnels sont constitués d'une multitude de structures précises, bien typées, qui s'emboîtent de façon prévisible. Les arbres de syntaxe `rowan` sont uniquement constitués de "noeuds" et de "feuilles" génériques. Les feuilles sont les léxèmes fourni au parseur, et les noeuds sont des jetons additionnels permettant de classifier le groupe de noeuds/feuilles que forment les enfants d'un noeud.
  * Les arbres de syntaxe ne doivent perdre aucune information. Les arbres de syntaxe traditionnels ommettent souvent les espaces, les sauts de ligne et les commentaires, et il est même courant de les ignorer dès l'analyse lexicale. Ici, ces caractères "inutiles" sont gardés. Cela permet de restituer parfaitement le code source à partir d'un arbre de syntaxe, ce qui peut être utile lors de la création d'outils de formattage automatique.
    * On notera d'ailleurs que pour obtenir le code source, il suffit de parcourir l'arbre de syntaxe en profondeur et concaténer les représentations textuelles de chaque feuille. La séquence de lexèmes reste strictement dans le même ordre, on ne fait que grouper des éléments de l'arbre à différents niveaux de profondeur.
  * Les erreurs sont tolérées. Si le parseur lit un léxème innatendu dans un contexte donné (ou n'en lit pas), il peut émettre un noeud `Error` et placer le léxème dedans, signalant aux étapes suivantes que le léxème doit être ignoré.
    * Il est très important de ne pas "jeter" le léxème innatendu mais de bien le placer quelque part dans l'arbre, afin de toujours pouvoir restituer le code source malgré l'erreur.
    * Il est possible de créer plusieurs tags `Error` différents pour typer/classifier les types d'erreurs. Dans notre cas, nous ne typons pas nos erreurs car les descriptions des erreurs sont émises directement au moment du parsing dans une liste à part. Le seul but du noeud `Error` est de mettre un token à part pour ne pas qu'il soit malencontreusement interprété par les étapes suivantes.

## Construire l'arbre

Maintenant que nous avons la structure de données, il reste à traduire une séquence de lexèmes en un arbre `rowan`. Nous n'avons aucune restriction particulière quant au type de parseur à utiliser. Par simple préférence, nous utilisons la bibliothèque [`nom`](https://lib.rs/crates/nom), qui suggère une architecture de parseurs basés sur des [combinateurs](https://en.wikipedia.org/wiki/Parser_combinator) et fournit beaucoup de combinateurs utiles.

L'architecture de `nom` n'est pas nativement conçue pour parser des arbres rowan. Les combinateurs `nom` renvoient beaucoup de types Rust très divers alors qu'un arbre rowan n'est constitué que de léxèmes et de noeuds nommés. Il faut donc re-créer quelques parseurs `nom` usuels adaptés. Voici des parseurs tels que `nom` les fournit:

| Combinateur          | Description                      | Signature `nom`                                        |
|----------------------|----------------------------------|--------------------------------------------------------|
| `opt(P)`             | Exécute le parseur P [0; 1] fois | `fn(Parser<T>) -> Parser<Option<T>>`                   |
| `many0(P)`           | Exécute le parseur P [0; ∞] fois | `fn(Parser<T>) -> Parser<Vec<T>>`                      |
| `tuple((P, Q, ...))` | Exécute les parseurs en chaîne   | `fn(Parser<T>, Parser<U>, ...) -> Parser<(T, U, ...)>` |

_Une liste non exhaustive mais plus longue de combinateur nom est (mieux) documentée [ici](https://github.com/rust-bakery/nom/blob/main/doc/choosing_a_combinator.md)_

Afin que tous ces parseurs renvoient le même type de valeur, on introduit un type `Children` qui représente une liste de noeuds/léxèmes rowan. Ce type sera renvoyé par tous les parseurs.

  * `opt` essaye d'exécuter le parseur passé en paramètre. En cas de succès, il renvoie directement la valeur de retour de son parseur. En cas d'échec, il renvoie un `Children` vide.
  * `many0` exécute plusieurs fois le parseur qu'on lui donne et fusionne par accumulation chaque `Children` produit à chaque exécution, en un seul `Children` qui sera retourné.
  * `tuple` fusionne également les `Children` renvoyés par ses parseurs, la différence étant qu'il prend une "liste" finie et non homogène de parseurs. Nous le renommerons `join` pour ne pas que le type de retour soit incohérent.

On définit également un combinateur `node(tag: Token, parser: P)` qui exécute le parseur une seule fois et englobe ses `Children` dans un noeud rowan de "tag" spécifié. Il renvoie un `Children` qui contient comme élément unique ce nouveau noeud.

Tous ces nouveux parseurs de compatibilité rowan/nom sont définis dans un module sobrement appelé `rowan-nom`, qui est suffisamment découplé du reste du code pour pouvoir être facilement -- ulterieurement -- extrait vers une autre crate Rust indépendante de `rustre`, pour pouvoir servir dans d'autres projets.

## Simplification des requêtes vers l'arbre de syntaxe

<!-- TODO ungrammar -->
