Steps to reproduce:

```
$ nix build
$ result/bin/gls-exe
First run:
Set up session: 0.002155565000
Set context: 0.031667107000
Run decls: 3.711375929000
Compile expr: 2.130753714000
ghcLoadApp: 5.876013324000

Second run:
Set up session: 0.001424016000
Set context: 0.013176788000
Run decls: 0.012871536000
Compile expr: 0.002616573000
ghcLoadApp: 0.030115064000
```

The first run is two orders of magnitude slower.

Non NixOS setup:
```
; ghcup install ghc 9.6.2
; ghcup set ghc 9.6.2
; git clone https://github.com/int-index/ghc-load-slow.git
; cd ghc-load-slow
; CABAL_DIR="$(pwd)/platform-cabal/" cabal install --lib gls-platform
; GLS_PKGDB="$(pwd)/platform-cabal/store/ghc-9.6.2/package.db" GLS_LIBDIR="$HOME/.ghcup/ghc/9.6.2/lib/ghc-9.6.2/lib/" cabal run gls-exe
```
You will need `pkg-config`, `libsdl2-dev` and ghcup installed
