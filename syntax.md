arithmetic

```ocaml
def x = 10 + 3 * 5

x ^ 3
```

functions

```ocaml
def f = \x -> x + 1

def g = \x -> x * 2

g (f 10)
```

recursive functions

```ocaml
def fact = \n -> fix \fact ->
  if n == 0 then 1
  else n * fact (n - 1)

def fib = \n -> fix \fib ->
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)

fact 10
```

side-effects

```ocaml
def loop = \n -> fix \loop ->
  when n > 0 then
    println "Hello, World!"
    loop (n - 1)

loop 10
```
