OC = ocamlc -c
OO = ocamlc -o
OLEX = ocamllex
OYACC = ocamlyacc
CXXFLAGS = -g

OBJS = jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo ir3_structs.cmo jlite_annotatedtyping.cmo jlite_toir3.cmo ir3_to_arm.cmo arm_structs.cmo
OCAML_OBJECTS = $(OBJS) jlite_main.cmo
OCAML_OBJECTS_IR3 = $(OBJS) jlite_main_ir3.cmo
TEST_DIR = test/

all:  $(patsubst %.jlite, %.s, $(wildcard $(TEST_DIR)*.jlite)) $(patsubst %.jlite, %.ir3, $(wildcard $(TEST_DIR)*.jlite)) $(patsubst %.jlite, %.sim, $(wildcard $(TEST_DIR)*.jlite))  

%.sim: %.exe
	./adb push $^ /data/local/tmp; \
	./adb shell ./data/local/tmp/$^  # Run the executable in adb and exit
	touch $@
	
%.exe: %.s
	./g++ $(CXXFLAGS) $< -o $@
	
%.s: %.jlite z.exe 
	./z.exe $< > $@

%.ir3: %.jlite zir3.exe
	./zir3.exe $< > $@
	
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

jlite_parser.cmo: jlite_parser.mly jlite_structs.cmo
	$(OYACC) $<; \
	$(OC) jlite_parser.mli; \
	$(OC) jlite_parser.ml

jlite_toir3.cmo: jlite_toir3.ml jlite_structs.cmo ir3_structs.cmo jlite_annotatedtyping.cmo
	$(OC) $<

ir3_structs.cmo: ir3_structs.ml jlite_structs.cmo
	$(OC) $<

jlite_main.cmo: jlite_main.ml ir3_to_arm.cmo jlite_toir3.cmo
	$(OC) $<

jlite_main_ir3.cmo: jlite_main_ir3.ml ir3_to_arm.cmo jlite_toir3.cmo
	$(OC) $<

clean:
	rm -f *.cmo *.cmi jlite_parser.ml jlite_lexer.ml *.s *.exe jlite_parser.mli *.ir3 *.sim

%.cmo : %.ml
	$(OC) $<
