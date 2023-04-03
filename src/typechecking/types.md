# Type-checking

Lustre est un langage statiquement et strictement typé. Chaque terme d'une équation et chaque expression a un type fixé, qui peut être déterminer avant l'exécution. Le typage statique a pour avantage majeur par rapport au typage dynamique de réduire considérablement les risques d'erreurs lors de l'exécution, et permet usuellement d'exécuter les programmes plus rapidement en évitant de devoir vérifier le type des valeurs pendant l'exécution.

Il existe beaucoup de mises en œuvre de type-checkers d'un langage à l'autre. Certains, comme celui d'OCaml, ont des capacités très puissantes d'inférence de type, c'est-à-dire que le type d'une valeur (un paramètre de fonction, par exemple) peut être déterminé en fonction de son usage.

```ocaml
let add a b = a + b;
```
_OCaml devine que la fonction `add` prend deux `int` en paramètre, car c'est ce que l'opérateur `+` requiert._

Heureusement pour nous, Lustre est un langage beaucoup plus simple. Les types des arguments des fonctions sont déclarés explicitement, et les types des expressions sont déterminés par leur parcours en profondeur uniquement — pas dans l'autre sens.

## Résilience

Le but de Rustre étant d'être le plus résilient possible, nous utilisons une approche au type-checking qui convient à cette exigence. Voici la représentation actuelle d'un type :

```rust
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub enum Type {
    /// Returned when an identifier cannot be resolved, or from an operator when it cannot resolve
    /// its type due to an operand being unknown to.
    #[default]
    Unknown,

    Boolean,
    Integer,
    Real,
    Function {
        args: Vec<Type>,
        ret: Vec<Type>,
    },
    Array {
        elem: Box<Type>,
        size: usize,
    },

    /// Tuple value or return type of a function with multiple (or 0) values
    ///
    /// A tuple **cannot** contain only one element, but **may** be empty. More specifically, if a
    /// function returns exactly one value, it **mustn't** be typed as a `ReturnTuple` as this would
    /// prevent it from being used as an operand to pretty much all operators.
    Tuple(Vec<Type>),
}
```

La partie intéressante est la variante `Type::Unknown`. Essentiellement, l'idée est que lorsqu'un type ne peut pas être déterminé à cause d'une erreur de l'utilisateur·ice (variable inexistante, fonction inexistante, opérateur appliqué à des opérandes illégaux...), on émet un diagnostic approprié et on renvoie ce type `Unknown`. Ensuite, celui-ci se comportera comme un joker, évitant qu'une erreur dans une expression cause une cascade d'erreurs dans toutes les expressions qui en dépendent.

## Indices de types

Très souvent, il est possible d'avoir une idée plus ou moins précise du type que doit avoir une expression _depuis l'extérieur_ (i.e. depuis un nœud parent dans l'AST). Par exemple, ici, on pourrait déterminer le type de `a + b` sans connaître le type des deux opérandes.

```lustre
[...]
var sum : real;
let
  sum = a + b;
tel;
```

Cette information peut s'avérer utile pour produire des messages d'erreur toujours plus personnalisés et précis. Nous mettons ça en œuvre en passant en paramètre de notre query de résolution de type un éventuel type "attendu". Celui-ci n'est jamais utilisé pour résoudre le type de l'expression passée en paramètre, mais uniquement à des fins d'amélioration des diagnostics.

Voici un exemple de ce que permet ce changement :

```lustre
function squared (n : int) returns (r : int);
let
  r = n^2; -- confusion d'opérateur : n^2 créé un tableau de 2 éléments qui valent n
tel;
```

Comme le résolveur de type — et en particulier la branche qui gère l'opérateur `^` — sait que le type attendu pour `r` est un `int`, on peut gérer cette confusion possible avec un message sur-mesure, plutôt qu'un bête `type mismatch: expected int, found int^2` :

```
Warning: possible confusion
   ╭─[tests/adder.lus:4:10]
   │
 4 │     r = n^2;
   │         ─┬─
   │          ╰─── the `^` operator creates an array by repeting an element a given number of times, it is not a power operator (hint: use ** instead)
───╯
```
