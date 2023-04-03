arithmetic

```ocaml
let x = 10 + 3 * 5

in x ^ 3
```

functions

```ocaml
let f = \x -> x + 1

let g x = x * 2

g (f 10)
```

recursive functions

```ocaml
let rec fact = \n ->
  if n == 0 then 1
  else n * fact (n - 1)

let rec fib n =
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)

fact 10
```

side-effects

```ocaml
let rec loop n =
  when n > 0
    let _ = println "Hello, World!" in
    loop (n - 1)
```

