#! /usr/bin/make -f

CC=cl65 -t c64 -I $$PWD
CFLAGS=-Os $(CDEBUG)
AS=ca65 -t c64 -I $$PWD
ASFLAGS=$(ASDEBUG)
LD=cl65 -t c64
LDFLAGS=-C c64-asm.cfg
TASS=64tass
ifdef LIBDIR
 LDFLAGS+=-L $(LIBDIR)
 ASFLAGS+=-I $(LIBDIR)
 CFLAGS+=-I $(LIBDIR)/include
 LIBT7D=$(LIBDIR)/libT7D.lib
 TASSFLAGS=-I$(LIBDIR)
else
 TASSFLAGS=-I/usr/share/cc65
endif
ifdef DEBUG
 CFLAGS += -DDEBUG=$(DEBUG)
 ASFLAGS += -DDEBUG=$(DEBUG)
endif
TARGET=animated-squiral

VPATH = src:assets
LIBT7D ?= libT7D.lib

ALLOBJS = $(notdir $(patsubst %.s, %.o, $(wildcard src/*.s))) squiral.large.o $(notdir $(patsubst %.c, %.o, $(wildcard src/*.c)))
OBJS = $(filter-out vsp.%, $(filter-out c64lzp%, $(ALLOBJS)))

all:	temporary.electricity.12.qadz datasrc.rauch.pu globals.i Around.tumbled.raw smiley-sprites.4X4.2000.raw credits-screen.raw skeleton-heroine.raw guitar-heroine.prg guitar-heroine.raw skeleton-heroine.prg credits-screen.prg $(TARGET).prg

vsp.prg:	$(filter vsp.%, $(ALLOBJS)) guitar-heroine.prg
	$(LD) -m $(basename $@).map -Ln $(basename $@).label -o temporary.$@ $(filter %.o, $^) $(LIBT7D)
	exomizer sfx systrim -o $@ temporary.$@

lzpcrunch:	$(filter c64lzp%, $(ALLOBJS))
	$(LD) --start-addr 0x4000 -m $(basename $@).map -Ln $(basename $@).label -o temporary.$@ $(filter %.o, $^) $(LIBT7D)
	#pucrunch -x 0x4000 temporary.$@ $@
	subsizer -o$@ -x -Xjmp=0x4000 temporary.$@

%.pu:	%.asm
	64tass -o temporary.$@ $<
	pucrunch -c0 temporary.$@ $@

%.prg: %.png
	png2prg -m hires $<

%.raw: %.prg
	head -c 8002 $< > temporary.bitmap.$(<F)
	xipz -a qadz -r temporary.bitmap.$(<F)
	mv temporary.bitmap.$(<F).raw $@

%.raw: %.bin
	xipz -a qadz -r $<
	mv $<.raw $@

%.raw: %.64c
	xipz -a qadz -r $<
	mv $<.raw $@

%.tmp: %.png
	graphconv --stucki $<
	mv $(patsubst %.png, %.c64, $<) $@

temporary.electricity.12.qadz: guitar-heroine.tmp guitar-electricity.5.tmp guitar-electricity.4.tmp guitar-electricity.3.tmp guitar-electricity.2.tmp guitar-electricity.1.tmp guitar-electricity.0.tmp
	./tools/xor_qadz.py -c guitar-heroine.tmp guitar-electricity.1.tmp temporary.electricity.h0.qadz
	./tools/xor_qadz.py -c guitar-electricity.0.tmp guitar-electricity.1.tmp temporary.electricity.01.qadz
	./tools/xor_qadz.py -c guitar-electricity.1.tmp guitar-electricity.2.tmp temporary.electricity.12.qadz
	./tools/xor_qadz.py -c guitar-electricity.2.tmp guitar-electricity.3.tmp temporary.electricity.23.qadz
	./tools/xor_qadz.py -c guitar-electricity.3.tmp guitar-electricity.4.tmp temporary.electricity.34.qadz
	./tools/xor_qadz.py -c guitar-electricity.4.tmp guitar-electricity.5.tmp temporary.electricity.45.qadz

	./tools/xor_qadz.py guitar-heroine.tmp guitar-electricity.0.tmp temporary.electricity.h0
	./tools/xor_qadz.py guitar-electricity.0.tmp guitar-electricity.1.tmp temporary.electricity.01
	./tools/xor_qadz.py guitar-electricity.1.tmp guitar-electricity.2.tmp temporary.electricity.12
	./tools/xor_qadz.py guitar-electricity.2.tmp guitar-electricity.3.tmp temporary.electricity.23
	./tools/xor_qadz.py guitar-electricity.3.tmp guitar-electricity.4.tmp temporary.electricity.34
	./tools/xor_qadz.py guitar-electricity.4.tmp guitar-electricity.5.tmp temporary.electricity.45
	xipz -a lzp -d -r temporary.electricity.h0 temporary.electricity.h0.lzp
	xipz -a lzp -d -r temporary.electricity.01 temporary.electricity.01.lzp
	xipz -a lzp -d -r temporary.electricity.12 temporary.electricity.12.lzp
	xipz -a lzp -d -r temporary.electricity.23 temporary.electricity.23.lzp
	xipz -a lzp -d -r temporary.electricity.34 temporary.electricity.34.lzp
	xipz -a lzp -d -r temporary.electricity.45 temporary.electricity.45.lzp

globals.i:	config.ini
	ini2headers.py -CAS config.ini

$(TARGET).prg:	globals.i $(OBJS)
	$(LD) $(LDFLAGS) -m $(basename $@).map -Ln $(basename $@).label -o temporary.$@ $(filter %.o, $^) $(LIBT7D)
	exomizer sfx 0x801 -o $@ -x 'inc $$d020 dec $$d020' temporary.$@

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data temporary.* *.zx02
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
	rm -f *.d64 *.prg
	rm -f globals.? globals.sh
	rm -f lzpcrunch
