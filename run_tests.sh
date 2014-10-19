#! /bin/bash
rm z.exe

# Debug programs
ocamlc -c  jlite_structs.ml
ocamlyacc jlite_parser.mly
ocamlc -c  jlite_parser.mli
ocamllex jlite_lexer.mll
ocamlc -c  jlite_lexer.ml
ocamlc -c  jlite_parser.ml
ocamlc -c  ir3_structs.ml
ocamlc -c  jlite_simple_annotatedtyping.ml
ocamlc -c  jlite_to_ir3.ml
ocamlc -c  jlite_main.ml
ocamlc  -o z.exe jlite_structs.cmo jlite_simple_annotatedtyping.cmo jlite_lexer.cmo jlite_parser.cmo jlite_to_ir3.cmo  ir3_structs.cmo jlite_main.cmo


rm *.cmo *.cmi
rm jlite_parser.mli jlite_parser.ml jlite_lexer.ml

# The error output are displayed with the sdtout channel 2.

./z.exe assign2_testcases/f_duplicate_name.jlite 2> assign2_testcases_output/f_duplicate_name.output
./z.exe assign2_testcases/f_illegal_assign.jlite 2> assign2_testcases_output/f_illegal_assign.output
./z.exe assign2_testcases/f_illegal_if.jlite 2> assign2_testcases_output/f_illegal_if.output
./z.exe assign2_testcases/f_illegal_if_return.jlite 2> assign2_testcases_output/f_illegal_if_return.output
./z.exe assign2_testcases/f_illegal_overloading.jlite 2> assign2_testcases_output/f_illegal_overloading.output
./z.exe assign2_testcases/f_illegal_return.jlite 2> assign2_testcases_output/f_illegal_return.output
./z.exe assign2_testcases/f_illegal_shadowing.jlite 2> assign2_testcases_output/f_illegal_shadowing.output
./z.exe assign2_testcases/f_illegal_undefined_field.jlite 2> assign2_testcases_output/f_illegal_undefined_field.output
./z.exe assign2_testcases/f_illegal_undefined_md_call.jlite 2> assign2_testcases_output/f_illegal_undefined_md_call.output
./z.exe assign2_testcases/f_illegal_untyped_md_call.jlite 2> assign2_testcases_output/f_illegal_untyped_md_call.output
./z.exe assign2_testcases/p_legal_1.jlite > assign2_testcases_output/p_legal_1.output
./z.exe assign2_testcases/p_legal_2.jlite > assign2_testcases_output/p_legal_2.output
./z.exe assign2_testcases/p_legal_3.jlite > assign2_testcases_output/p_legal_3.output
./z.exe assign2_testcases/p_legal_binary.jlite > assign2_testcases_output/p_legal_binary.output
./z.exe assign2_testcases/p_legal_ex1.jlite > assign2_testcases_output/p_legal_ex1.output
./z.exe assign2_testcases/p_legal_fruitninja.jlite > assign2_testcases_output/p_legal_fruitninja.output
./z.exe assign2_testcases/p_legal_overloading.jlite > assign2_testcases_output/p_legal_overloading.output
./z.exe assign2_testcases/p_legal_shadowing.jlite > assign2_testcases_output/p_legal_shadowing.output
./z.exe assign2_testcases/sample1.jlite > assign2_testcases_output/sample1.output
./z.exe assign2_testcases/sample2.jlite > assign2_testcases_output/sample2.output
./z.exe assign2_testcases/sample3.jlite > assign2_testcases_output/sample3.output
