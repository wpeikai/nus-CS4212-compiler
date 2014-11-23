OC = ocamlc -c
OO = ocamlc -o
OLEX = ocamllex
OYACC = ocamlyacc

OCAML_OBJECTS = jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo jlite_toir3.cmo ir3_structs.cmo jlite_annotatedtyping.cmo ir3_to_arm.cmo arm_structs.cmo jlite_main.cmo
OCAML_OBJECTS_IR3 = jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo jlite_toir3.cmo ir3_structs.cmo jlite_annotatedtyping.cmo ir3_to_arm.cmo arm_structs.cmo jlite_main_ir3.cmo
JLITE_INPUT=input.jlite

all: arm.s
	./g++ $< -o out; \
	./adb push out /data/local/tmp; \
	./adb shell ./data/local/tmp/out  # Run the executable in adb and exit

arm.s: z.exe 
	./z.exe $(JLITE_INPUT) > $@; \
	# cp hello_world.s arm.s # To remove

ir3: zir3.exe
	./zir3.exe $(JLITE_INPUT) > input.ir3
	
z.exe: $(OCAML_OBJECTS)
	$(OO) $@ $+

zir3.exe: $(OCAML_OBJECTS_IR3)
	$(OO) $@ $+

jlite_lexer.cmo: jlite_lexer.ml jlite_parser.cmo
	$(OC) $<

jlite_lexer.ml: jlite_lexer.mll  jlite_parser.cmo
	$(OLEX) $<

ir3_to_arm.cmo: ir3_to_arm.ml ir3_structs.cmo arm_structs.cmo
	$(OC) $<

arm_structs.cmo: arm_structs.ml
	$(OC) $<

jlite_parser.cmo: jlite_parser.mly jlite_structs.cmo
	$(OYACC) $<; \
	$(OC) jlite_parser.mli; \
	$(OC) jlite_parser.ml

jlite_structs.cmo: jlite_structs.ml
	$(OC) $<

jlite_toir3.cmo: jlite_toir3.ml jlite_structs.cmo ir3_structs.cmo
	$(OC) $<

jlite_annotatedtyping.cmo: jlite_annotatedtyping.ml
	$(OC) $<

ir3_structs.cmo: ir3_structs.ml jlite_structs.cmo
	$(OC) $<

jlite_main.cmo: jlite_main.ml ir3_to_arm.cmo jlite_toir3.cmo
	$(OC) $<

jlite_main_ir3.cmo: jlite_main_ir3.ml ir3_to_arm.cmo jlite_toir3.cmo
	$(OC) $<

clean:
	-@rm *.cmo *.cmi jlite_parser.ml jlite_lexer.ml arm.s jlite_parser.mli out z.exe 2> /dev/null || true 