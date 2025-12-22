#! /usr/bin/make -f

AS=ca65 -t c64
LD=ld65
CC=cl65 -t c64
LIBT7DDIR ?= ../library
CLFLAGS=--asm-include-dir $(LIBT7DDIR)
CFLAGS=-Os -I$(LIBT7DDIR)/include
LDFLAGS=-L$(LIBT7DDIR)
ASFLAGS=-I$(LIBT7DDIR)
LIBT7D ?= $(LIBT7DDIR)/libT7D.lib

TARGETS =  takeover_bulleteffect.prg takeover_dissolve_left2right.prg takeover_curtain_left2right.prg
TARGETS += fsvscroll.skeleton_using_c.prg fsvscroll.skeleton_using_asm.prg
TARGETS += memory_copy_routine.prg
TARGETS += spritecanvas3X4.prg
TARGETS += frameengine.5q.prg

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY: all clean

all:	$(TARGETS)

clean:
	rm -f *.o *.labels *.map *.tmp
	rm -f $(TARGETS)
	rm -f memory_copy_routine
	rm -f spritecanvas3X4

takeover_%.prg: takeover_%.o
	$(LD) $(LDFLAGS) -C takeover_default.cfg -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib

fsvscroll.skeleton_using_c.prg:	fsvscroll.skeleton_using_c.o fsvscroll.example_frame.o lowcode_music.1000.o
	cl65 -t c64 -o $@ -Ln $@.labels $(CFLAGS) $(LDFLAGS) --asm-include-dir ../library -C fsvscroll.memorymap.cfg $^ libT7D.lib

fsvscroll.skeleton_using_asm.prg: fsvscroll.skeleton_using_asm.o fsvscroll.example_frame.o lowcode_music.1000.o
	cl65 -t c64 -o $@ -Ln $@.labels $(CFLAGS) $(LDFLAGS) --asm-include-dir ../library -C fsvscroll.memorymap.cfg $^ libT7D.lib

memory_copy_routine.prg: memory_copy_routine.s
	cl65 $(CLFLAGS) -t c64 $^ ../library/libT7D.lib
	xipz -a qadz -j 2061 -p 0x90 memory_copy_routine

spritecanvas3X4.prg:	spritecanvas3X4.s
	cl65 $(CLFLAGS) $^ $(LIBT7D)
	xipz -a qadz -j 2061 -p 0x90 spritecanvas3X4

frameengine.5q.prg:	frameengine.5q.s
	cl65 -o $@.tmp --start-addr 0x1000 -m $@.map -Ln $@.labels -t c64 -C c64-asm.cfg $(CLFLAGS) $^ $(LIBT7D)
	xipz -a qadz $@.tmp
	mv $@.tmp.prg $@
