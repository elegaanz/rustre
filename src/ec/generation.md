# Génération

La génération d'extended code (abrégé *EC* dans la suite) n'est pas documenté
par l'implémentation officielle. Cette page cherche donc à fournir une documentation
expliquant comment générer un tel code à partir d'un fichier Lustre écrit par
un⋅e utilisateurice. Elle se base sur le code de l'implémentation officielle et
l'observation de ses sorties.

## Types

Seuls les types de base (`int`/`bool`/`real`) sont conservés. Les types « anonymes » sont également conservés.

Pour les variables de type **tableau**, on remplace chaque élément du tableau par une variable unique.

```haskell
const x : int^3 = [1, 2, 3];

-- devient

const x_0 : int = 1;
const x_1 : int = 2;
const x_2 : int = 3;
```

Les **énumérations** sont remplacées par un type anonyme et un ensemble de constante externes.

```haskell
type e = enum { A, B };

const e_value : e = A;

-- devient

type e;
const A : e;
const B : e;

const e_value : e = A;
```

Les **structures** sont remplacées par des variables individuelles pour chacun de leur champs.

```haskell
type s = { x : int; y : real };
const s_value : s = s { x = 42; y = 3.1415 };

-- devient

const s_value_x = 42;
const s_value_y = 3.1415;
```

## Nœuds

Les expressions sont séparées pour obtenir un seul opérateur par équation/assertion.

```haskell
assert x and y and z;
z = x or y and (a < 12);

-- devient

assert_1 = x and y;
assert_2 = assert_1 and z;
assert assert_2;

a_lt_12 = a < 12;
x_or_y = x or y;
z = x_or_y and a_lt_12;
```

Les opérateurs suivants sont étendus : map, fill, red, fillred, boolred, condact, diese, merge, nor.

## Packages

À faire plus tard…

## Prise de note

Quand on compile un programme Lustre avec `lv6 -ec`, plusieurs options sont en réalité activées[^ec-options] :

- les options pour générer du code LV4 sont activées
    - les itérateurs sont inlinés (???) (`global_opt.inline_iterator <- true;`)
    - `when_on_ident <- true`
    - les arrays sont étendues (`opt.expand_arrays <- true`)
- les énumérations sont transformées en constantes
- on élimine les `when not` (`no_when_not`)
- on supprime les préfixes (`no_prefix`)
- on étend les nœuds (`opt.expand_nodes <- true`)

Une fois que ces options ont été activées, un fichier `.ec` contenant l'EC est écrit[^lic-to-file], puis la compilation termine.

### Expansion des énumérations

Les énumérations sont remplacées par des types externes, et chaque variante devient une constante externe de ce type. Un exemple est donné [ici].

### Types

<https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/lib/licPrg.ml###L151>

Comme on compile en mode « LV4 », les déclarations de types ne sont pas écrites (ce qui est un peu bizarre, ça semble logique de ne pas
le faire pour de l'EC vu qu'on simplifie tout en types élémentaires, mais pourquoi c'est le cas en LV4 aussi ?)

### Constantes

<https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/lib/licPrg.ml###L262-267>

Elles ne sont pas dans le fichier de sortie EC non plus.

### Nœuds

C'est là que c'est vraiment intéressant. Un fichier EC ne contient qu'un seul nœud.

Il peut cependant contenir des nœuds externes. Ceux déclarés dans le fichier source sont donc recopiés.

La déclaration du nœud en elle-même ne change pas, mais son corps (si il existe) est transformé.[^node-of-node-exp-eff]

Les assertions ne sont pas changées.



[^ec-options]: <https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/lib/lv6MainArgs.ml#L259>

[^lic-to-file]: <https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/bin/lv6/main.ml#L309-L311>

[^node-of-node-exp-eff]: <https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/lib/licDump.ml#L788>

[ici]: https://gricad-gitlab.univ-grenoble-alpes.fr/verimag/synchrone/lustre-v6/-/blob/ad176d2a7891cc6b12b98c107f210e03b86d5d24/lib/licPrg.ml#L167-184
