Steps to reproduce:

```
$ nix build
$ result/bin/gls-exe
First run:
Set up session: 0.002940159000
Set context: 0.038944889000
Run decls: 0.669186427000
Compile expr: 0.794986964000
ghcLoadApp: 1.506118165000

Second run:
Set up session: 0.001910596000
Set context: 0.026225599000
Run decls: 0.019831534000
Compile expr: 0.003311731000
ghcLoadApp: 0.051315081000
```

The first run is an order of magnitude slower.
