#!/usr/bin/env bash

rm -rf ./.bin
mkdir ./.bin
ocamlc -o \
  ./.bin/program \
  ./interpreter.ml
