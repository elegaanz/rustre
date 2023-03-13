# Initialisation des variables et des paramètres

En Lustre, toutes les valeurs de retour et les variables intermédiaires doivent être initialisées.

```lustre
function double (x : real) returns (y : real);
let
  y = 2. * x;
tel;
```

Si on omettait l'équation `y = 2. * x;`, `lv6` nous renverrait cette erreur:

```
Error: in file "double.lus", line 1, col 5 to 10, token 'double':
Error: 
*** "y" (output y:real on base(y,0)) is not defined. 
*** Defined variables are: 
  - 
```

A première vue, vérifier quelle valeur est assignée et quelle valeur ne l'est pas peut paraître simple. Mais Lustre permet des cas de figure beaucoup plus complexes. Prenons par exemple une modélisation d'un [_registre à décalage_](https://en.wikipedia.org/wiki/Shift_register), un composant eléctronique courant qui peut s'apparenter à une file de valeurs booléennes de taille constante.

```lustre
node shift_register (input : bool) returns (queue : bool^8 ; carry_out : bool);
let
  queue[0] = input;
  queue[1..8] = false^7 -> pre queue[0..7];
  carry_out = pre queue[8];
tel;
```

> Pour expliquer rapidement, ligne par ligne:
>   * On insère l'input en tête de queue
>   * On definit les 7 valeurs restantes comme étant
>       * que des `false` au cycle 0
>       * puis les 7 premières valeurs de la file au cycle précédent respectif, pour tous les cycles suivants
>   * On définit la retenue sortante comme étant la dernière valeur de la file au cycle précédent

On remarque plusieurs choses dans ce code quant à l'initialisation des variables :
  * D'une part, nous avons plusieurs variables à initialiser. Cela reste un cas relativement simple à gérer.
  * D'autre part, la variable `queue` est composite (ou plutôt son type). Il s'agit d'un tableau de taille constante dont chaque élément doit être initialisé individuellement et il faut donc pouvoir gérer l'_initialisation partielle_ d'une valeur. En particulier, ce qui est intéressant est de savoir **si une variable est parfaitement initialisée**, et sinon **quels "morceaux" de la variable ne le sont pas** (afin d'emmettre des messages d'erreur précis).

Afin de respecter la spécification, il est important que `rustre` vérifie que les variables sont intégralement initialisées. En plus du respect de la spécification, cela fournit également des garanties sur lesquelles des étapes sous-jacentes pourront reposer. Par exemple, si on veut compiler un programme Lustre en langage machine (e.g. pour de la compilation JIT), il est probable que nous passions par le back-end LLVM, qui considère les accès à de la mémoire non initialisé comme un _comportement indéfini_ (UB). En vérifiant préalablement que toutes les variables sont bien affectées, on s'épargne la nécéssité de les initialiser avec une valeur quelconque (par exemple 0).

## Approche par développement des variables composites

La première approche qui a été considérée est de simplement développer les variables dites "composites" (structures et tableaux). Par exemple, une variable `color : int^3` pourrait être développée en `color_0 : int`, `color_1 : int` et `color_2 : int`. Il est alors très simple de vérifier que les variables originales ont été intégralement initialisées en vérifiant que les variables développées les sont toutes individuellement.

Il est probable que le compilateur officiel fonctionne comme ça, car cette expansion des valeurs composites est notamment effectuée lors de l'expansion du code `.lus` en `.ec` (extended code).

Cette approche a deux inconvénients:

  * Il est plus compliqué de reporter des messages d'erreur clairs lorsqu'une variable n'est pas bien initialisée, et on se retrouve presque à réimplémenter (à l'envers) l'algorithme de la deuxième approche (ci-dessous)
  * L'empreinte mémoire et processeur peut être inutilement élevée pour des programmes utilisant des grosses structures de données. Par exemple, une simple fonction identité qui prend une structure développable en 10000 champs demanderait un travail énorme à être vérifiée alors qu'il s'agit d'une tâche triviale.

## Approche par arbre de partition

Notre approche n'est pas très différente de la précédente, sauf que le développement des variables en éléments composites fait de manière paresseuse (à la manière d'un quadtree par exemple). L'idée est la suivante:

> Partons d'une variable de retour `pixels : rgb^5^3` avec `type rgb = { r, g, b: int }`, que nous souhaitons initialiser.
> On s'apprête à itérer sur toutes les équations d'un noeud Lustre. On commence par créer un ensemble de variables à initialiser (il s'agit en pratique des variables de retour ainsi que des `var` déclarées dans le noeud). Ici il n'y a qu'une valeur de retour.
>
> ```
> pixels
> ```
>
> Maintenant, nous itérons sur les équations. Admettons que la première d'entre elle définisse à 0 le canal vert du pixel haut-gauche de l'écran: `pixels[0][0].g = 10`. L'ensemble de variable à initialiser ressemble donc maintentant à ça:
>
> ```
> pixels[0][0].r
> pixels[0][0].b
> pixels[1]
> pixels[2]
> pixels[3]
> pixels[4]
> ```
>
> Et ainsi de suite, on ne garde dans cet ensemble que les "morceaux" de variables pas encore initialisés. Lorsque toutes les équations ont été parcourues, il est alors très simple de voir si toutes les variables sont effectivement initialisées (ensemble vide) et sinon, lesquelles restent à affecter.

Cette vérification nécéssite d'avoir accès au type des variables, et ne peut donc être effectuée qu'après une phase de type-checking. Notons également que l'ensemble de variables à initialiser peut également être représenté sous forme d'arbre, plutôt qu'un ensemble.
