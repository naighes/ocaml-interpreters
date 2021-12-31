#!/usr/bin/env bash

# ./parser.ml \
# ./interpreter.ml \
rm -rf ./.bin
mkdir ./.bin
ocamlc -o \
  ./.bin/program \
  ./lexer.ml \
  ./parser.ml \
  ./main.ml
