arithmetic

```ocaml
let x = 10 + 3 * 5

in x ^ 3
```

functions

```ocaml
let f = \x -> x + 1 in

in f 10
```

recursive functions

```ocaml
let rec fact = \n ->
  if n == 0 then 1
  else n * fact (n - 1)

in fact 10
```

side-effects

```ocaml
let rec loop () n =
  when n > 0
    let _ = println "Hello, World!" in
    loop (n - 1)
```

