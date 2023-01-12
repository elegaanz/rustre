Pour palier aux inconvénients de l'architecture traditionnelle des compilateurs,
on propose une nouvelle architecture basée sur :

1. un lexeur assez classique
2. un arbre de syntaxe qui hiérarchise les tokens dans un arbre,
  mais sans construire un AST dans lequel les informations sont synthétisées
3. un AST fonctionnel qui exploite les tokens dans l'arbre de syntaxe pour
  mettre en évidence la structure de l'arbre et les propriétés de chaque nœud
4. un système de **queries** (« demandes » / « requêtes ») inter-dépendantes

Pour celà, on s'appuie sur des crates déjà existantes, qui sont respectivement :

1. [logos]
2. [rowan] et [nom]
3. [ungrammar]
4. [salsa]

S'appuyer sur ces bibliothèques nous permet de gagner du temps, et de profiter
d'implémentations performantes et bien testées des algorithmes dont nous avons besoin.

## Arbre de syntaxe

Les arbres de syntaxe que l'on construit sont des arbres d'arité arbitraire,
où chaque nœud est étiqueté par un token.

Les tokens sont représentés par un type énuméré, sans données associée aux constructeurs.
Mais en plus des tokens produits par le lexeur (identifiant, mot-clé, constante, etc.),
cette énumération contient des variantes qui permettent de donner une sémantique à l'arbre
(« déclaration de fonction » par exemple).

Prenons le code suivant :

```ocaml
function id (x : bool) returns (f : bool)
let
    f = x;
tel;
```

Le lexeur produit une liste de token qui ressemble à :

```rust
Function
Ident // « id »
LeftParen
Ident // « x »
Colon
Bool
RightParen
Returns
LeftParen
Ident // « f »
Colon
Bool
RightParen
Let
Ident // « f »
Equal
Ident // « x »
Semicolon
Tel
Semicolon
```

Le « parseur » (étape 2 dans la liste donnée plus haut), va alors hiérarchiser les tokens
de cette manière :

```rust
FunctionDeclaration
  Function
  Ident // « id »
  Parameters
    LeftParen
    VariableDeclaration
      Ident // « x »
      Colon
      Bool
    RightParen
  Return
  Outputs
    LeftParen
    VariableDeclaration
      Ident // « f »
      Colon
      Bool
    RightParen
  Let
  FunctionBody
    Equation
      EquationLHS
        Ident // « f »
      Equal
      EquationRHS
        Ident // « x »
    Semicolon
  Tel
  Semicolon
```

## Naviguer dans l'arbre de syntaxe

Pour parcourir cette arbre de syntaxe facilement, on met en place une API fonctionnelle.

Les différentes fonctions de parcours de l'arbre étant très répétitives, on génère ce code
à l'aide d'`ungrammar`. Cette crate permet à partir d'un fichier décrivant une grammaire,
de générer du code source Rust arbitraire au moment de la compilation.

Par exemple, cette règle ungrammar :

```
Root = IncludeStatement* PackageBody? PackageList?
```

Genère ce code (pas besoin de le lire en détail, faut juste voir qu'avec une petite ligne on peut
générer beaucoup de code répétitif) : 

```rust
// Root

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Root {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for Root {
    fn can_cast(kind: Token) -> bool {
        kind == Token::Root
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
    fn expect(syntax: SyntaxNode) -> Self {
        Self::cast(syntax).expect("Failed to cast to Root")
    }
}

impl Root {
    pub fn all_include_statement(&self) -> impl Iterator<Item = IncludeStatement> {
        self.syntax().children().filter_map(IncludeStatement::cast)
    }
    pub fn package_body(&self) -> Option<PackageBody> {
        self.syntax().children().find_map(PackageBody::cast)
    }
    pub fn package_list(&self) -> Option<PackageList> {
        self.syntax().children().find_map(PackageList::cast)
    }
}
```

## Manipuler l'arbre : le système de queries

Le système de queries est ce qui va permettre d'implémenter les différentes passes
du compilateur.

Une query peut être vue comme une fonction : elle prend des paramètres en entrée,
et calcule un résultat de sortie. Cependant cette fonction doit être pure : si les
entrées sont les mêmes, alors les sorties seront les mêmes. De cette manière, on
peut mettre en cache le résultat des queries et éviter de refaire plusieurs fois
le même calcul.

Une query va généralement dépendre du résultat d'autres queries, qu'elle peut
demander à exécuter. Mais contrairement à un système de passes classique,
c'est **la query qui indique de quoi elle dépend**, et pas le système qui lui
passe le résultat des passes précédentes en entrée.

Le système fonctionne donc « à l'envers » : on lance la query « génère du binaire »,
qui va demander à exécuter les passes précendentes, en remontant jusqu'aux premières passes
(« construit un arbre de syntaxe pour tel fichier »).

Pour certaines queries, il est intéressant d'écrire leur cache sur le disque
en plus de la mémoire, pour pouvoir les réutiliser la prochaine fois que le
compilateur est lancé.

## Avantages de cette architecture

On peut reconstruire facilement une petite partie de l'arbre de syntaxe (utile
si on veut développer un « language server » ou un plugin d'IDE qui réagit dès
que le code est modifié dans l'éditeur).

On ne perd aucune information : tous les tokens sont préservés, les commentaires
et les espaces peuvent rester dans l'AST, sans pour autant nous gêner la plupart
du temps (utile si on veut développer un formatteur automatique ou un générateur
de documentation, mais aussi pour afficher des erreurs pertinentes).

C'est possible de créer un arbre de syntaxe partiellement valide. Si on arrive pas à
parser quelques tokens, on peut les englober dans un nœud « Error » et passer à la suite.

On peut enrichir l'AST facilement en ajoutant des queries, sans avoir à redéfinir
des représentations intermédiaires.

Lustre ayant des sorties variées (extended code, binaire, C, etc.), le système de queries
est particulièrement adapté. On définit une query par type de sortie, et on les laisse
définir leur dépendances, sans avoir à redéfinir toute la suite de passes à chaque fois.

Lustre étant un langage pur, on peut définir une query d'interprétation d'un nœud/fonction
et mettre ses résultats en cache, pour créer un interpréteur intégré au compilateur qui
soit efficace.

[logos]: https://crates.io/crates/logos
[rowan]: https://crates.io/crates/rowan
[nom]: https://crates.io/crates/nom
[ungrammar]: https://crates.io/crates/ungrammar
[salsa]: https://crates.io/crates/salsa