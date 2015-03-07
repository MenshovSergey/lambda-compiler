# lambda-compiler

## Description

Simple compiler of some imperative language to lambda calculus. Currently generates Lisp-style AST.

It supports ariphmetic and comparison operators, numbers and variables.

## Build

    ocamlbuild main.byte

## Run

    ./main.byte < test-expr.txt

## Todo

- [x] Read string from stdin (currently hardcoded)
- [x] Implement power
- [ ] Merge logical operations
- [ ] Implement variable assignment
- [ ] Implement multiple statements (``;`` operator)
