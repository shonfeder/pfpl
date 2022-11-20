# Chapters 4 and 5

Introduction to statics and dynamics, using the simple language *E*.

E has a tiny and trivial set of expressions and types. For which, see
[./e.ml](./e.ml).

## Type checking

An example of a syntactically correct and well-typed program:

```sh
$ cat ./well_typed_ex.e
let z : Num =
    let x : Num = 2 in
    let y : Num = 3 in
    x + (y + 5)
in

let a : Str = "foo" in

let b : Str = "bar" in

a ++ " " ++ b
```

We can confirm it is well-typed by running the typechecker:

```sh
$ e typecheck < ./well_typed_ex.e
```

An example of an ill-typed program:

```sh
$ cat ./ill_typed_ex.e
let n : Num = 2 + 2 in
let s : Str = "4" in
n + s
```

We can confirm it is ill-typed:


```sh
$ e typecheck < ./ill_typed_ex.e
Type Error:
  given:
    s : Str
  expected:
    s : Num
  in context:
n : Num
s : Str
```

## Evaluation

We can evaluate a well typed program

```sh
$ e eval < ./well_typed_ex.e
"foo bar"
```
