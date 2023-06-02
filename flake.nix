{
  description = "ghc-load-low - reproduce a ghc api perf bug";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc962";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          shelly = pkgs.haskell.lib.dontCheck (hself.callHackage "shelly" "1.12.1" {});
          gls-platform = haskellPackages.callCabal2nix "gls-platform" "${self}/src/platform/" {};
          gls-exe = haskellPackages.callCabal2nix "gls-exe" "${self}/src/exe/" {};
        });
      glsRuntimeEnv = haskellPackages.ghcWithPackages(p: [p.gls-platform]);
      GLS_LIBDIR = "${glsRuntimeEnv}/lib/ghc-${glsRuntimeEnv.version}/lib";
    in
    {
      packages.${system}.ghc-load-slow =
        pkgs.stdenv.mkDerivation {
          name = "ghc-load-slow-with-env";
          nativeBuildInputs = [ pkgs.makeWrapper ];
          buildCommand = ''
            mkdir -p $out/bin
            makeWrapper "${haskellPackages.gls-exe}/bin/gls-exe" "$out/bin/gls-exe" \
              --set GLS_LIBDIR ${GLS_LIBDIR}
          '';
        };

      defaultPackage.${system} = self.packages.${system}.ghc-load-slow;

      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          glsRuntimeEnv
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ];
        inherit GLS_LIBDIR;
      };
    };
}
