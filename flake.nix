{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let
      package = "tapl";
      makeLegacyPackages = system: isDev:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          on = opam-nix.lib.${system};
          query =
            if isDev
            then {
              # dev tools must be relayed here to ensure they are built with
              # the same compiler version and dependencies
              merlin = null;
              ocaml-lsp-server = null;
            }
            else { };
          scope = on.buildDuneProject { } package ./. query;
          overlay = self: super:
            {
              # Your overrides go here
            };
        in
        scope.overrideScope' overlay;
    in
    flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages = makeLegacyPackages system false;
      defaultPackage = self.legacyPackages.${system}.${package};
      devShell =
        let
          pkgs = nixpkgs.legacyPackages.${system};
          legacyPackages = makeLegacyPackages system true;
        in
        pkgs.mkShell {
          # Derived from: https://github.com/tweag/opam-nix/issues/1
          nativeBuildInputs = (legacyPackages.nativeBuildInputs or [ ]) ++ [
            # dev tools
            pkgs.ocamlformat
            legacyPackages.merlin
            legacyPackages.ocaml-lsp-server
          ];
          inputsFrom = [ legacyPackages.${package} ];
        };
    });
}
