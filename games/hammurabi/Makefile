#! /usr/bin/make -f

ifeq ($(OS),Windows_NT)
    $(error Error: This Makefile is intended to be run inside a Docker container. Use the script build_win.bat)
endif

T7DLIB ?= libT7D.lib
AS = ca65 -t c64
CL = cl65 -t c64
CC = cl65 -t c64
LD = cl65 -t c64
CFLAGS = -O
ifdef T7DINCLUDE
CLFLAGS += --asm-include-dir $(T7DINCLUDE)
ASFLAGS += -I $(T7DINCLUDE)
endif
DISKFILES = hammurabi $(DISKFILESADD)
DISKFILESADD = title font documentation main

RESIDENT_STARTADDR = 0x400

all:	dload.prg resident.prg hammurabi.d64

hammurabi.d64:	$(DISKFILES)
	rm -f $@
	cc1541 -n hammurabi -i " 2024" -w hammurabi -f "#63#63#63#63#63#63#63#63#63#63#63#63#63#63#63#63" -T DEL -L $(addprefix -w , $(DISKFILESADD)) -f "#63#63#63#63#63#63#63#63#63#63#63#63#63#63#63#63" -T DEL -N -m -L $@
	dart dirart.d64 -o hammurabi.d64 -s all

resident.prg:	resident.s
	$(CL) -o $@ -m $(basename $@).map -l $(basename $@).lst -C c64-asm.cfg --start-addr $(RESIDENT_STARTADDR) --asm-define RESIDENTADDR=$(RESIDENT_STARTADDR) $(CLFLAGS) $^ $(T7DLIB)

hammurabi:	hammurabi.s
	$(CL) -o $@.prg $(CLFLAGS) $^ $(T7DLIB)
	exomizer sfx systrim -o $@ $@.prg

documentation.prg: documentation-asm.o documentation.o
	$(LD) --start-addr 0x2000 -m $@.map -o $@ $^

main.prg: main-asm.o main.o
	$(LD) --start-addr 0x2000 -m $@.map -o $@ $^

%.o: %.s
	$(AS) $(ASFLAGS) -o $@ $^

#Skip build-in rule to assemble source to binary directly.
%: %.s

%: %.prg
	@echo -ne '\x1b[42m'
	tail -c +3 $^ > $^.tmp
	zx02 -f $^.tmp
	(echo -n ~~; cat $^.tmp.zx02) > $@
	rm $^.tmp $^.tmp.zx02
	@echo -ne '\x1b[0m'

%.prg: %.o
	@echo -ne '\x1b[32m'
	cl65 $(CLFLAGS) --start-addr 0x2000 -t c64 -C c64-asm.cfg -m $@.map -o $@ $^ $(T7DLIB)
	@echo -ne '\x1b[0m'

dload.prg:
	cp dload.cfg /usr/src/dreamload
	make -C /usr/src/dreamload dload.prg
	cp $(addprefix /usr/src/dreamload/, dload.prg dload-labels.inc) .

.PHONY:	clean
clean:
	rm -f hammurabi.d64 hammurabi $(DISKFILES)
	rm -f loader-c64.prg install-c64.prg loadersymbols-c64.inc
	rm -f dload.prg dload-labels.inc
	rm -f *.prg *.map *.o
