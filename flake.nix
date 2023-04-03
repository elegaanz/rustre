{
  description = "Shell with mdbook and typst";

  inputs = {
    typst.url = "github:typst/typst";
  };

  outputs = { self, nixpkgs, typst }: let pkgs = nixpkgs.legacyPackages.x86_64-linux; in {
    devShell.x86_64-linux = pkgs.mkShell {
      nativeBuildInputs = [ typst.packages.x86_64-linux.default pkgs.mdbook pkgs.mdbook-katex pkgs.mdbook-plantuml pkgs.python3 ];
    };
  };
}
