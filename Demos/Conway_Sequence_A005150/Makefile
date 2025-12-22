#! /usr/bin/make -f

CC=cl65 -t c64 -I $$PWD
CFLAGS=-Os $(CDEBUG)
AS=ca65 -t c64 -I $$PWD
ASFLAGS=$(ASDEBUG)
LD=cl65 -t c64
LDFLAGS=-C c64-2200.cfg
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
TARGET=cowaseq

VPATH = src:assets
LIBT7D ?= libT7D.lib

all:	$(TARGET).prg

cowaseq.prg:	cowaseq.o
	$(LD) --start-addr 0x2200 -C c64-zpmod.cfg -o temporary.2200.$@ -Ln $@.label -m $@.map  $(filter %.o, $^) $(LIBT7D) LAMAlib.lib
	prepender64 -j 0x2200 --copy-eor -o temporary.full.$@ temporary.2200.$@ Dark_future.sid,0x1000,0x7c Star_Maker.upper.64c,0x800
	exomizer sfx systrim -o $@ temporary.full.$@

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data temporary.* *.zx02
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
	rm -f dload.prg dload-labels.inc
	rm -f boot resident  $(ZFILES)
	rm -f *.d64
	rm -f *.prg
	rm -f globals.? globals.sh
