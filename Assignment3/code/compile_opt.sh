ocamlc -c optimize_arm.ml
ocamlc -o optimize_arm jlite_structs.cmo ir3_structs.cmo arm_structs.cmo optimize_arm.cmo
./optimize_arm