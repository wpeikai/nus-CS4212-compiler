#! /bin/bash
rm z.exe

ocamlc -c jlite_structs.ml
ocamlyacc jlite_parser.mly
ocamlc -c jlite_parser.mli
ocamllex jlite_lexer.mll
ocamlc -c jlite_lexer.ml
ocamlc -c jlite_parser.ml
ocamlc -c ir3_structs.ml
ocamlc -c jlite_simple_annotatedtyping.ml
ocamlc -c jlite_main.ml
ocamlc -o z.exe jlite_structs.cmo jlite_simple_annotatedtyping.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_main.cmo

rm *.cmo *.cmi
rm jlite_parser.mli jlite_parser.ml jlite_lexer.ml

# ./z.exe assign2_testcases/f_duplicate_name.jlite
# ./z.exe assign2_testcases/f_illegal_assign.jlite
# ./z.exe assign2_testcases/f_illegal_if.jlite
# ./z.exe assign2_testcases/f_illegal_if_return.jlite
# ./z.exe assign2_testcases/f_illegal_overloading.jlite
# ./z.exe assign2_testcases/f_illegal_return.jlite
# ./z.exe assign2_testcases/f_illegal_shadowing.jlite
# ./z.exe assign2_testcases/f_illegal_undefined_field.jlite
# ./z.exe assign2_testcases/f_illegal_undefined_md_call.jlite
# ./z.exe assign2_testcases/f_illegal_untyped_md_call.jlite
./z.exe assign2_testcases/p_legal_1.jlite
./z.exe assign2_testcases/p_legal_2.jlite
./z.exe assign2_testcases/p_legal_3.jlite
./z.exe assign2_testcases/p_legal_binary.jlite
./z.exe assign2_testcases/p_legal_ex1.jlite
./z.exe assign2_testcases/p_legal_fruitninja.jlite
./z.exe assign2_testcases/p_legal_overloading.jlite
./z.exe assign2_testcases/p_legal_shadowing.jlite

# ./z.exe ex1.txt
# ./z.exe simple_input.j
rm z.exe
