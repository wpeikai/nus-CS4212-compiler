OC = ocamlc -c
OO = ocamlc -o
OLEX = ocamllex
OYACC = ocamlyacc

all : jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo jlite_toir3.cmo ir3_structs.cmo \
	jlite_annotatedtyping.cmo jlite_main.cmo
	$(OO) z.exe jlite_structs.cmo jlite_annotatedtyping.cmo jlite_lexer.cmo jlite_parser.cmo jlite_toir3.cmo  ir3_structs.cmo jlite_main.cmo

jlite_lexer.cmo : jlite_lexer.ml jlite_parser.cmo
	$(OC) jlite_lexer.ml

jlite_lexer.ml : jlite_lexer.mll  jlite_parser.cmo
	$(OLEX) jlite_lexer.mll

jlite_parser.cmo : jlite_parser.ml
	$(OC) jlite_parser.ml

jlite_parser.ml : jlite_parser.mli jlite_structs.cmo
	$(OC) jlite_parser.mli

jlite_parser.mli : jlite_parser.mly
	$(OYACC) jlite_parser.mly

jlite_structs.cmo : jlite_structs.ml
	$(OC) jlite_structs.ml

jlite_toir3.cmo : jlite_toir3.ml jlite_structs.cmo ir3_structs.cmo
	$(OC) jlite_toir3.ml

jlite_annotatedtyping.cmo : jlite_annotatedtyping.ml
	$(OC) jlite_annotatedtyping.ml

ir3_structs.cmo : ir3_structs.ml jlite_structs.cmo
	$(OC) ir3_structs.ml

jlite_main.cmo : jlite_main.ml
	$(OC) jlite_main.ml

clean :
	\rm *.cmo *.cmi jlite_parser.mli jlite_parser.ml jlite_lexer.ml