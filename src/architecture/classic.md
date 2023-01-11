# Architecture classique d'un compilateur

Avant de présenter l'architecture du compilateur Rustre, nous allons présenter l'architecture
classique d'un compilateur, afin d'en exposer les limites et expliquer pourquoi nous
avons décidé de ne pas adopter cette approche.

Un compilateur est un programme qui prend du code source (lisible et rédigé par des humains) en entrée,
et qui génère un fichier binaire (généralement exécutable). Dans le cas de Lustre, avec les outils officiels,
on peut :

- générer l'*extended code* d'un programme (inlining de tous les nœuds pour n'en avoir plus qu'un seul)
- générer du code C équivalent à un programme
- générer un binaire équivalent à un programme
- générer un automate équivalent à un programme
- simuler un programme (sans compiler son code source)
- faire du model-checking sur un programme

Ici, on va se concentrer sur le cas où on veut générer un binaire.

## Passes et représentations intermédiaires

Un compilateur se divise en plusieurs « **passes** » (comprendre « étapes »)
successives, ayant chacune un rôle précis dans la manipulation du code source.
Chaque passe utilise ses propres structures de données pour se représenter le code
source, appelées « **représentations intermédiaires** » (en réalité, plusieurs passes
consécutives peuvent utiliser la même représentation intermédiaire).

Généralement, on distingue ces passes :

- le **lexeur**, prend le code source sous forme de texte et produit une liste de
  tokens (~ mots) distints
- le **parseur**, utilise la liste des tokens pour construire un arbre représentant
  de manière abstraite le programme
- différentes passes de **vérification** (qui dépendent du langage et de ses règles),
  par exemple vérifier que les types sont cohérents
- différentes passes d'**optimisation**
- une passe finale qui génère du binaire

## Lexeur

Le lexeur prend en entrée du code source et génère une liste de tokens, pré-découpant
le code source pour faciliter son analyse. Cette étape élimine (généralement) des informations
inutiles comme les espaces ou les commentaires.

On peut imaginer un lexeur Lustre qui prendrait ce code source :

```ocaml
const n = 4;

-- Multiplexeur à deux entrées
function multiplex (a, b, s : bool) returns (f : bool)
let
    f = (a and s) or (b and not s);
tel;
```

Et qui renvoie cette liste de tokens :

```rust
Const, Ident("n"), Egal, Int(4), PointVirgule,
Function, Ident("multiplex"), ParenGauche,
Ident("a"), Virgule, …
```

## Parseur

Le parseur construit une représentation intermédiaire qu'on appelle AST (Abstract Syntax Tree).
C'est une représentation du code source sous forme d'arbre.

Un parseur Lustre pourrait produire l'arbre suivant pour le code source précédent.

```rust
FichierLustre {
    declarations: [
        Constante { nom: "n", valeur: Entier(4) },
        Fonction {
            nom: "multiplex",
            entrees: [
                ("a", Bool),
                ("b", Bool),
                ("c", Bool),
            ],
            sorties: [
                ("f", Bool),
            ],
            equations: [
                "f", Or(
                    And(Var("a"), Var("s")),
                    And(Var("b"), Not(Var("s"))),
                ),
            ],
        },
    ],
}
```

## Passes suivantes

Les passes suivantes vont vérifier certaines propriétés sur l'AST, le transformer
et l'enrichir de nouvelles informations (ce qui demandera d'autres représentations intermédiaires,
mais qui resteront généralement sous forme d'arbres).

Ces passes sont dans un ordre bien défini, et vont « en avant » (on exécute la première passe,
puis on passe son résultat à la seconde, et ainsi de suite).

## Inconvénients de cette approche

On a peu de résilience face aux erreurs : concevoir un AST qui peut être partiellement valide
n'est pas possible (ou du moins, demande un travail spécial).
Une erreur dans une passe fait s'arrêter la compilation entièrement, même si une partie du programme est valide
et aurait pu être analysée d'avantage.

On doit également multiplier les représentations intermédiaires. Par exemple si on veut ajouter le type de chaque
variable et de chaque expression, on doit redéfinir entièrement chaque type de l'AST. Puis, si dans une passe suivante
on veut ajouter un compteur du nombre d'utilisation de chaque variable/fonction/nœud (pour émettre un avertissement si
certains sont définis sans jamais être utilisés), on doit à nouveau redéfinir l'AST avec ce nouveau champ dans chaque
type.

Rajouter une passe peut être compliqué, surtout dans le cas où on développe des outils parallèles au compilateur (plugin
d'IDE, linter).