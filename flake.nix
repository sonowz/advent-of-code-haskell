{
  description = "Advent of Code Haskell solutions";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        version = "1.0.0";
        name = "advent-of-code-haskell";

        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskell.packages.ghc925.ghc;  # The version should match with resolver!
        requirements = pkgs.callPackage ./requirements.nix {};
        devRequirements = with pkgs; [
          stack-wrapped
          cabal-install

          (haskell-language-server.override { supportedGhcVersions = [ "925" ]; })
          haskellPackages.ormolu
          hlint
        ];

        # https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          version = pkgs.stack.version;
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        defaultPackage = pkgs.stdenv.mkDerivation {
          inherit name;
          inherit version;
          src = ./.;
          installPhase = "echo 'dummy app'";
        };

        devShell = (pkgs.haskell.lib.buildStackProject {
          inherit name;
          inherit version;
          inherit ghc;
          stack = stack-wrapped;
          buildInputs = requirements;
        }).overrideAttrs (final: prev: {
          buildInputs = prev.buildInputs ++ devRequirements;
        });
      }
    );
}
