#! /usr/bin/make -f

AS=ca65
ASFLAGS=-t c64
LIBS = --lib c64.lib llamalib.lib
LD=cl65
LDFLAGS=-C c64-basicfriendly-asm.cfg -u __EXEHDR__

SOURCES=$(wildcard *.s)
OBJS=$(SOURCES:.s=.o)
TARGETS=$(OBJS:.o=.prg)

BFFILES=$(wildcard *.bf.prg)

.PHONY:	all clean

all:: lamabrainfuck.d64 
#$(TARGETS)

clean::
	rm -f *.o
	rm -f $(TARGETS)
	rm -f *.labels

%.prg: %.o 
	$(LD) -Ln $(basename $@).labels $(LDFLAGS) -o $@ $+ LAMAlib.lib

lamabrainfuck.d64: lamabrainfuck.prg
	cp lamabrainfuck_template.d64 lamabrainfuck.d64
	c1541 -attach $@ -write $<

run: lamabrainfuck.d64
	x64 lamabrainfuck.d64:

#		$(call c1541 -attach $@ -write ${bff} ),\