# meow

An implementation of [Gradual Typing for Functional Languages](http://scheme2006.cs.uchicago.edu/13-siek.pdf).

I started writing this on [International Cat Day](https://en.wikipedia.org/wiki/International_Cat_Day) and decided to name the repo `meow` ;).

Some meow-difications from the paper:
- "meow", "😺", "😸", "😹", "😻", "😼", "😽", "🙀", "😿", "😾" are aliases for the `?` type
- "meow", "😺", "😸", "😹", "😻", "😼", "😽", "🙀", "😿", "😾" are also commands! They print out a random cat emoji ;)

You will need [sedlex](https://github.com/ocaml-community/sedlex/) and [menhirLib](https://gitlab.inria.fr/fpottier/menhir/) to build the project (in addition to the OCaml toolchain of course, >= 14.4.0).

## Running the examples

Evaluate file:
```
dune exec bin/main.exe -- examples/<name-of-file>
```

Only run the typechecker
```
dune exec bin/main.exe -- examples/<name-of-file> --typecheck
```

Dump Cast AST
```
dune exec bin/main.exe -- examples/<name-of-file> --dump-cast
```
