CCIR3 = ../code/zir3.exe
CC = ../code/z.exe
# objects = $(wildcard *.txt)
# CFILES = $(shell ls | grep .txt)
# OBJS = (CFILES:%.txt=%.ir3)

all: test_6params.ir3 test_booleans.ir3 test_fields.ir3 test_functions1.ir3 test_functions.ir3 test_ops.ir3 \
test_6params.s test_booleans.s test_fields.s test_functions1.s test_functions.s test_ops.s

test_6params.ir3: test_6params.txt
	$(CCIR3) $< > $@

test_booleans.ir3: test_booleans.txt
	$(CCIR3) $< > $@

test_fields.ir3: test_fields.txt
	$(CCIR3) $< > $@

test_functions1.ir3: test_functions1.txt
	$(CCIR3) $< > $@

test_functions.ir3: test_functions.txt
	$(CCIR3) $< > $@

test_ops.ir3: test_ops.txt
	$(CCIR3) $< > $@

test_6params.s: test_6params.txt
	$(CC) $< > $@

test_booleans.s: test_booleans.txt
	$(CC) $< > $@

test_fields.s: test_fields.txt
	$(CC) $< > $@

test_functions1.s: test_functions1.txt
	$(CC) $< > $@

test_functions.s: test_functions.txt
	$(CC) $< > $@

test_ops.s: test_ops.txt
	$(CC) $< > $@

clean:
	@rm *.ir3 *.s