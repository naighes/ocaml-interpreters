#!/usr/bin/env bash

rm -rf ./.bin
mkdir ./.bin
ocamlc -o \
  ./.bin/program \
  ./lexer.ml \
  ./parser.ml \
  ./interpreter.ml \
  ./main.ml
