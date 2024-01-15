#! /usr/bin/make -f

AS=ca65 -t c64
LD=ld65
CC=cl65 -t c64
CFLAGS=-Os -I../library/include
LDFLAGS=-L../library
ASFLAGS=-I../library

TARGETS =  takeover_bulleteffect.prg takeover_dissolve_left2right.prg takeover_curtain_left2right.prg
TARGETS += fsvscroll.skeleton_using_c.prg fsvscroll.skeleton_using_asm.prg
TARGETS += memory_copy_routine.prg

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY: all clean

all:	$(TARGETS)

clean:
	rm -f *.o *.labels *.map
	rm -f $(TARGETS)
	rm -f memory_copy_routine

takeover_%.prg: takeover_%.o
	$(LD) $(LDFLAGS) -C takeover_default.cfg -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib

fsvscroll.skeleton_using_c.prg:	fsvscroll.skeleton_using_c.o fsvscroll.example_frame.o lowcode_music.1000.o
	cl65 -t c64 -o $@ -Ln $@.labels $(CFLAGS) $(LDFLAGS) --asm-include-dir ../library -C fsvscroll.memorymap.cfg $^ libT7D.lib

fsvscroll.skeleton_using_asm.prg: fsvscroll.skeleton_using_asm.o fsvscroll.example_frame.o lowcode_music.1000.o
	cl65 -t c64 -o $@ -Ln $@.labels $(CFLAGS) $(LDFLAGS) --asm-include-dir ../library -C fsvscroll.memorymap.cfg $^ libT7D.lib

memory_copy_routine.prg: memory_copy_routine.s
	cl65 --asm-include-dir ../library -t c64 $^ ../library/libT7D.lib
	xipz -a qadz -j 2061 -p 0x90 $^

