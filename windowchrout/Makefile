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

all:: test_drawframe.prg test_frame_and_window.prg

clean::
	rm -f *.o
	rm -f $(TARGETS)
	rm -f *.labels

test_drawframe.prg: test_drawframe.o window_parameters.o draw_frame.o
	$(LD) -Ln $(basename $@).labels $(LDFLAGS) -o $@ $+ LAMAlib.lib

test_frame_and_window.prg: test_frame_and_window.o window_parameters.o draw_frame.o window_chrout.o
	$(LD) -Ln $(basename $@).labels $(LDFLAGS) -o $@ $+ LAMAlib.lib