#! /usr/bin/make -f

AS=ca65
ASFLAGS=-t c64
LIBS = --lib c64.lib llamalib.lib
LD=cl65
LDFLAGS=-C c64-basicfriendly-asm.cfg -u __EXEHDR__

SOURCES=$(wildcard *.s)
OBJS=$(SOURCES:.s=.o)
TARGETS=$(OBJS:.o=.prg)

.PHONY:	all clean

all:: $(TARGETS)

clean::
	rm -f *.o
	rm -f $(TARGETS)
	rm -f *.labels

%.prg: %.o
	$(LD) -Ln $(basename $@).labels $(LDFLAGS) -o $@ $+ LAMAlib.lib
