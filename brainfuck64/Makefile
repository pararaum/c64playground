#! /usr/bin/make -f

CC=cl65 -t c64
CFLAGS=-O -I $(LIBDIR)/include $(CDEBUG)
AS=ca65
ASFLAGS=-t c64 -I $(LIBDIR) -I $(LIBDIR) $(ASDEBUG)
LD=ld65
LDFLAGS=-L $(LIBDIR)
AR=ar65
LIBDIR=../library
X64=x64
X64FLAGS=+truedrive -device8 1 -fs8 . -autostartprgmode 1
CRUNCHER=pucrunch
CRUNCHERFLAGS=

BFLIB = libbrainfuck/brainfuck.lib

.PHONY:	all disk test run clean debug presentation

all:	libbrainfuck/brainfuck.lib brainfuck64.prg disk

disk:	brainfuck64.prg *.bf
	cp logo.d64 brainfuck64.d64
	cc1541 -n brainfuck -i " vcc " $(foreach I,$+,-w $(I)) brainfuck64.d64

prg:	$(PROJECT).prg
	$(CRUNCHER) $(CRUNCHERFLAGS) $(PROJECT) $(PROJECT).prg

test:

debug:
	$(MAKE)	ASDEBUG="-DDEBUG" CDEBUG="-DDEBUG"

run:	brainfuck64.prg
	$(X64) $(X64FLAGS) -autostart $+

$(BFLIB):
	make -C libbrainfuck

clean:
	make -C libbrainfuck clean
	rm -f brainfuck64.prg basicfuck.prg *.o *.map *.labels
	rm -f *.raw
	rm -f brainfuck64.d64
	rm -f presentation.html

distclean:	clean
	rm -f *~

brainfuck64.raw: basicfuck.o isr_showmem.o irq-catcher.o $(BFLIB)
	$(LD) -C basicmap.cfg -m $@.map -Ln $@.labels $(LDFLAGS) -o $@ $+ libT7D.lib LAMAlib.lib

%.prg:	%.raw
	$(CRUNCHER) $(CRUNCHERFLAGS) -x0x7800 $+ $@

presentation:
	darkslide presentation.md
