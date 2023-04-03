# Rustre's documentation

[Project report](https://projets-info4.gricad-pages.univ-grenoble-alpes.fr/22-23/26/docs/report.pdf)

## Build it

A nix shell is provided with mdbook and typst to build the docs.

```bash
nix develop # or nix-shell if you don't have flakes
mdbook serve # or mdbook build
typst watch report/main.typ report.pdf # or typst compile …
```
