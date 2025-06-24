include ../../../../etc/Makefile.conf

PROG = interp
EADK_APP_NAME = "camlboot"
DEVICE ?= numworks

# FIXME: find what is the upper limit for these?
STACK_SIZE = 16192
HEAP_SIZE = 16192

TARGETS := $(PROG).hex $(PROG).elf

INCLUDES := -I stdlib/ -I parsing/ -I interpreter/
CINCLUDES := $(shell echo $(INCLUDES) | sed -e 's/ /,/g')
OMICROB_PARAMS := -v -mlopts $(CINCLUDES),-g,-ccopt=-DEADK_APP_NAME=$(EADK_APP_NAME) -device=$(DEVICE)

OCAMLOPT = ocamlopt.opt

# Define intermediate files
BYTECODE = interp.byte
C_SOURCE = interp.c

# Directory containing the source files
SRCS += stdlib/*.mli stdlib/*.ml
SRCS += parsing/*.mli parsing/*.ml
SRCS += interpreter/*.mli interpreter/*.ml
SSRCS = $(shell ocamldep -bytecode -sort $(INCLUDES) $(SRCS))

MLS = $(filter %.ml,$(SSRCS))
CMOS = $(patsubst %.ml,%.cmo,$(MLS))
MLIS = $(filter %.mli,$(SSRCS))
CMIS = $(patsubst %.mli,%.cmi,$(MLIS))

stdlib/lexing.cmi: stdlib/lexing.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/lexing.mli

stdlib/lexing.cmo: stdlib/lexing.cmi stdlib/lexing.ml
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/lexing.ml

stdlib/warnings.cmi: stdlib/lexing.cmi stdlib/warnings.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/warnings.mli

stdlib/warnings.cmo: stdlib/lexing.cmo stdlib/warnings.cmi stdlib/warnings.ml
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/warnings.ml

stdlib/format.cmi: stdlib/format.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/format.mli

parsing/location.cmi: stdlib/warnings.cmi stdlib/lexing.cmi stdlib/format.cmi parsing/location.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c parsing/location.mli

parsing/location.cmo: stdlib/warnings.cmo stdlib/lexing.cmo stdlib/format.cmi parsing/location.cmi parsing/location.ml
	$(BIN)/omicrob $(OMICROB_PARAMS) -c parsing/location.ml

parsing/asttypes.cmi : parsing/location.cmi parsing/asttypes.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c parsing/asttypes.mli

stdlib/longident.cmi : stdlib/longident.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/longident.mli

stdlib/longident.cmo : stdlib/longident.cmi stdlib/longident.ml
	$(BIN)/omicrob $(OMICROB_PARAMS) -c stdlib/longident.ml

parsing/parsetree.cmi: stdlib/longident.cmi parsing/asttypes.cmi parsing/parsetree.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c parsing/parsetree.mli

interpreter/data.cmo: parsing/parsetree.cmi parsing/asttypes.cmi interpreter/data.ml
	$(BIN)/omicrob $(OMICROB_PARAMS) -c interpreter/data.ml

testsmap.cmo: testsmap.ml interpreter/data.cmo
	$(BIN)/omicrob $(OMICROB_PARAMS) -c testsmap.ml

.PHONY: testsmap
testsmap: testsmap.byte testsmap.c testsmap.elf
	@echo "Compilation de testsmap terminée."

testsmap.byte: interpreter/data.cmo testsmap.cmo
	$(BIN)/omicrob $(OMICROB_PARAMS) interpreter/data.cmo testsmap.cmo -o testsmap.byte

testsmap.c: testsmap.byte
	$(BIN)/omicrob -v -device=$(DEVICE) testsmap.byte -o testsmap.c

testsmap.elf: testsmap.c
	$(BIN)/omicrob -v -device=$(DEVICE) testsmap.c -o testsmap.elf

.PHONY: all
all: hex elf
	file $(PROG).byte
	file $(PROG).hex
	cp -vf $(PROG).hex $(PROG).nwa
	file $(PROG).nwa

.PHONY: hex
hex: $(PROG).hex
	cp -vf $(PROG).hex $(PROG).nwa

.PHONY: elf
elf: $(PROG).elf

$(TARGETS): $(CMOS) camlboot.cmo
	@echo "Compiling .cmo files to .byte"
	$(BIN)/omicrob $(OMICROB_PARAMS) -stack-size $(STACK_SIZE) -heap-size $(HEAP_SIZE) $^ -o $@

%.cmi: %.mli
	$(BIN)/omicrob $(OMICROB_PARAMS) -c $<

%.cmo: %.ml $(CMIS)
	$(BIN)/omicrob $(OMICROB_PARAMS) -c $<

.PHONY: clean
clean:
	rm -vf **/*.cm*
	rm -vf *.hex *.elf *.nwa *.byte

NWLINK = npx --yes -- nwlink@0.0.17

.PHONY: run
run: $(TARGETS)
	@echo "INSTALL $<"
	$(Q) $(NWLINK) install-nwa $<
