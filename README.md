# Algorithm W Reimplementation

This project reimplements Martin Grabmuller's **Algorithm W Step by Step** in
Haskell, with a small source parser and command-line interface.

The implemented language supports:

- variables: `x`
- integer and boolean literals: `1`, `True`, `False`
- lambda abstraction: `\x -> x`
- function application: `f x`
- let polymorphism: `let id = \x -> x in id`
- `if ... then ... else ...`
- simple binary operators: `+`, `-`, `*`, `==`, `&&`, `||`

## Build and Run

```bash
cabal run algorithm-w
cabal run algorithm-w -- expr "let id = \x -> x in id 3"
cabal run algorithm-w -- file examples/id.aw
```

If `cabal run` is affected by local Cabal log permissions on Windows, compile
directly with GHC:

```bash
ghc -package containers -isrc app/Main.hs -o algorithm-w
./algorithm-w
./algorithm-w expr "let id = \x -> x in id 3"
./algorithm-w file examples/id.aw
```

## Project Structure

- `src/Syntax.hs`: expression AST and binary operators
- `src/Types.hs`: types, schemes, substitutions, environments, errors
- `src/Infer.hs`: unification and Algorithm W
- `src/Parser.hs`: a small hand-written parser
- `app/Main.hs`: CLI and built-in examples

## Core Ideas

Algorithm W recursively walks the expression AST. It creates fresh type
variables for unknown types, uses unification to solve constraints, and uses
`generalize` / `instantiate` to implement let-polymorphism.
