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

all:: greetings.inc $(TARGETS)

clean::
	rm -f *.o
	rm -f $(TARGETS)
	rm -f *.labels

%.prg: %.o 
	$(LD) -Ln $(basename $@).labels $(LDFLAGS) -o $@ $+ LAMAlib.lib

greetings.inc: greetings.txt prepare-text.py
	python prepare-text.py greetings.txt > greetings.inc

text-fly-in.prg: text-fly-in.s greetings.inc
	cl65 -Ln text-fly-in.labels -C c64-basicfriendly-asm.cfg -u __EXEHDR__ -o text-fly-in.prg text-fly-in.s LAMAlib.lib

run: text-fly-in.prg
	x64 text-fly-in.prg
	