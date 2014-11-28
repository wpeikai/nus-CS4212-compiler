OC = ocamlc -g -c
OO = ocamlc -g -o
OLEX = ocamllex
OYACC = ocamlyacc
CXXFLAGS = -g
ADB = ./adb
GG = ./g++
ZZ = ./z.exe
ZZIR3 = ./zir3.exe

OBJS_COMMON = arm_structs.cmo jlite_lexer.cmo jlite_parser.cmo jlite_structs.cmo ir3_structs.cmo jlite_annotatedtyping.cmo 
OBJS_OPTIM = $(OBJS_COMMON) jlite_toir3.cmo liveness.cmo ir3_to_arm.cmo optimize_arm.cmo
OBJS_SIMPLE = $(OBJS_COMMON) jlite_toir3_non_optim.cmo ir3_to_arm_non_optim.cmo jlite_main_non_optim.cmo
OCAML_OBJECTS = $(OBJS_OPTIM) jlite_main.cmo
OCAML_OBJECTS_IR3 = $(OBJS_OPTIM) jlite_main_ir3.cmo
TEST_DIR = test/

JLITE_FILES_TO_TEST = $(patsubst %.output, %.test, $(wildcard $(TEST_DIR)*.output))


all:  $(patsubst %.jlite, %.ir3, $(wildcard $(TEST_DIR)*.jlite)) $(patsubst %.jlite, %.sim, $(wildcard $(TEST_DIR)*.jlite))  

noptim:  $(patsubst %.jlite, %.noptim, $(wildcard $(TEST_DIR)*.jlite))  

test: $(JLITE_FILES_TO_TEST)

%.noptim: %.noptim_exe
	$(ADB) push $^ /data/local/tmp/$(TEST_DIR) 1> /dev/null 2>&1; \
	$(ADB) shell ./data/local/tmp/$^ 1> $@;  # Run the executable in adb and exit
	touch $@
		
%.noptim_exe: %.S
	$(GG) $(CXXFLAGS) $< -o $@

%.sim: %.exe
	$(ADB) push $^ /data/local/tmp/$(TEST_DIR) 1> /dev/null 2>&1; \
	$(ADB) shell ./data/local/tmp/$^ 1> $@;  # Run the executable in adb and exit
	touch $@

%.test: %.sim %.output
	@cmp $^

%.exe: %.s
	$(GG) $(CXXFLAGS) $< -o $@
	
%.S: %.jlite z_noptim.exe 
	$(ZZ) $< > $@

%.s: %.jlite z.exe 
	$(ZZ) $< > $@

%.ir3: %.jlite zir3.exe
	$(ZZIR3) $< > $@
	
z.exe: $(OCAML_OBJECTS)
	$(OO) $@ $+

z_noptim.exe: $(OBJS_SIMPLE)
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

jlite_main.cmo: jlite_main.ml ir3_to_arm.cmo jlite_toir3.cmo optimize_arm.ml
	$(OC) $<

optimize_arm.cmo: optimize_arm.ml jlite_structs.cmo ir3_structs.cmo arm_structs.cmo
	$(OC) $<

jlite_main_ir3.cmo: jlite_main_ir3.ml ir3_to_arm.cmo jlite_toir3.cmo
	$(OC) $<

clean:
	@rm -f *.cmo *.cmi jlite_parser.ml lite_lexer.ml $(TEST_DIR)/*.s $(TEST_DIR)/*.exe jlite_parser.mli $(TEST_DIR)/*.ir3 $(TEST_DIR)/*.sim

%.cmo: %.ml
	@$(OC) $<
