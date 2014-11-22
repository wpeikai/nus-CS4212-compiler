OC = ocamlc -c
OO = ocamlc -o
OLEX = ocamllex
OYACC = ocamlyacc
OBJS = jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo jlite_toir3.cmo ir3_structs.cmo \
	jlite_annotatedtyping.cmo jlite_main.cmo


all : z.exe

z.exe: $(OBJS)
	$(OO) $@ $^

jlite_lexer.cmo : jlite_lexer.ml jlite_parser.cmo
	$(OC) jlite_lexer.ml

jlite_lexer.ml : jlite_lexer.mll  jlite_parser.cmo
	$(OLEX) jlite_lexer.mll

jlite_parser.ml : jlite_parser.mli jlite_structs.cmo
	$(OC) jlite_parser.mli

jlite_parser.mli : jlite_parser.mly
	$(OYACC) jlite_parser.mly

jlite_toir3.cmo : jlite_toir3.ml jlite_structs.cmo ir3_structs.cmo
	$(OC) jlite_toir3.ml

ir3_structs.cmo : ir3_structs.ml jlite_structs.cmo
	$(OC) ir3_structs.ml

%.cmo : %.ml
	$(OC) $^

clean :
	rm -f *.cmo *.cmi jlite_parser.mli jlite_parser.ml jlite_lexer.ml
