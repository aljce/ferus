# ferus
An OCaml clone (for now)

# building
```shell
nix-shell --pure
cargo build
```

# example
```
> (fn f => (fn x => f (fn v => x x v)) (fn x => f (fn v => x x v)))
  (fn f => fn n => if n = 0 then 1 else n * f (n - 1)) 10
3628800
```
