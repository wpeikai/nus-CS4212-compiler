#!/bin/bash

ocamlc -c jlite.ml

ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamllex lexer.mll

ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
ocamlc -o parser lexer.cmo jlite.cmo parser.cmo main.cmo
rm parser.mli lexer.ml parser.ml *.cmo *.cmi
