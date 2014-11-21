#! /bin/bash

ocamlc -c  jlite_structs.ml
ocamlyacc jlite_parser.mly
ocamlc -c  jlite_parser.mli
ocamllex jlite_lexer.mll
ocamlc -c  jlite_lexer.ml
ocamlc -c  jlite_parser.ml
ocamlc -c  ir3_structs.ml
ocamlc -c  jlite_annotatedtyping.ml
ocamlc -c  jlite_toir3.ml
ocamlc -c  jlite_main.ml

ocamlc -o z.exe jlite_structs.cmo jlite_annotatedtyping.cmo jlite_lexer.cmo jlite_parser.cmo jlite_toir3.cmo  ir3_structs.cmo jlite_main.cmo

rm *.cmo *.cmi
rm jlite_parser.mli jlite_parser.ml jlite_lexer.ml
