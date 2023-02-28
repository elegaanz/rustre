# Rustre

Rustre est un compilateur [Lustre] écrit en Rust.
Le but de ce projet est de fournir un compilateur plus agréable à utiliser
que le compilateur officiel (meilleurs messages d'erreur, meilleure documentation,
interface en ligne de commande plus intuitive), mais aussi d'explorer
de nouvelles idées pour le design de compilateurs, s'inspirant notamment
des travaux de [Rust Analyzer] et de [rustc].

Éventuellement, nous aimerions aussi fournir un framework pour manipuler du code source
Lustre, permettant de développer des outils en plus du compilateur (formattage de code par exemple).

Enfin, nous pensons que Lustre a du potentiel en tant que langage embarqué, c'est à dire qu'un interpréteur pourrait être intégré à un logiciel pour pouvoir lui développer des plugins en Lustre. Or, l'interpréteur officiel étant developpé en OCaml, il est beaucoup plus compliqué à intégrer à un programme que le serait une bibliothèque Rust ou C compilée en langage machine.

[Lustre]: https://www-verimag.imag.fr/The-Lustre-Programming-Language-and
[Rust Analyzer]: https://rust-analyzer.github.io/
[rustc]: https://github.com/rust-lang/rust/
