{
  description = "ghc-load-low - reproduce a ghc api perf bug";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc944";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          sdl2 = pkgs.haskell.lib.dontCheck (hself.callHackage "sdl2" "2.5.4.0" {});
          hspec-contrib = hself.callHackage "hspec-contrib" "0.5.1.1" {};
          gls-platform = haskellPackages.callCabal2nix "gls-platform" "${self}/src/platform/" {};
          gls-exe = haskellPackages.callCabal2nix "gls-exe" "${self}/src/exe/" {};
        });
      glsRuntimeEnv = haskellPackages.ghcWithPackages(p: [p.gls-platform]);
      GLS_LIBDIR = "${glsRuntimeEnv}/lib/ghc-${glsRuntimeEnv.version}";
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
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
        inherit GLS_LIBDIR;
      };
    };
}
