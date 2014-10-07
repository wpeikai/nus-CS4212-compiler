#!/bin/bash

./parser tests/jlite_files_no_pass/test_no_pass.jlite > tests/trees/tree_test_no_pass.txt 2> tests/errors_tests_no_pass/error_test_no_pass.txt


./parser tests/jlite_files/test0.jlite > tests/trees/tree_test0.txt 2> tests/errors/error_test0.txt
./parser tests/jlite_files/test1.jlite > tests/trees/tree_test1.txt 2> tests/errors/error_test1.txt
./parser tests/jlite_files/test6.jlite > tests/trees/tree_test6.txt 2> tests/errors/error_test6.txt
./parser tests/jlite_files/test_assoc.jlite > tests/trees/tree_test_assoc.txt 2> tests/errors/error_test_assoc.txt
./parser tests/jlite_files/test_boolean_operator.jlite > tests/trees/tree_test_boolean_operator.txt 2> tests/errors/error_test_boolean_operator.txt
./parser tests/jlite_files/test_class_empty.jlite > tests/trees/tree_test_class_empty.txt 2> tests/errors/error_test_class_empty.txt
./parser tests/jlite_files/test_comments.jlite > tests/trees/tree_test_comments.txt 2> tests/errors/error_test_comments.txt
./parser tests/jlite_files/test_empty_class.jlite > tests/trees/tree_test_empty_class.txt 2> tests/errors/error_test_empty_class.txt
./parser tests/jlite_files/test_no_class.jlite > tests/trees/tree_test_no_class.txt 2> tests/errors/error_test_no_class.txt
./parser tests/jlite_files/test_no_method_in_class.jlite > tests/trees/tree_test_no_method_in_class.txt 2> tests/errors/error_test_no_method_in_class.txt
./parser tests/jlite_files/test_no_var_decl_in_class.jlite > tests/trees/tree_test_no_var_decl_in_class.txt 2> tests/errors/error_test_no_var_decl_in_class.txt
./parser tests/jlite_files/test_one_method.jlite > tests/trees/tree_test_one_method.txt 2> tests/errors/error_test_one_method.txt
./parser tests/jlite_files/test_one_var_one_method.jlite > tests/trees/tree_test_one_var_one_method.txt 2> tests/errors/error_test_one_var_one_method.txt
./parser tests/jlite_files/test_only_vardecl.jlite > tests/trees/tree_test_only_vardecl.txt 2> tests/errors/error_test_only_vardecl.txt
./parser tests/jlite_files/test_return_expr.jlite > tests/trees/tree_test_return_expr.txt 2> tests/errors/error_test_return_expr.txt
./parser tests/jlite_files/test_string_literal.jlite > tests/trees/tree_test_string_literal.txt 2> tests/errors/error_test_string_literal.txt
