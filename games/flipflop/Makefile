#! /usr/bin/make -f

CC=cl65 -t c64
CFLAGS=-Os $(CDEBUG)
AS=ca65 -t c64
ASFLAGS=$(ASDEBUG)
LD=cl65 -t c64
LDFLAGS=-C memorymap.cfg
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
TARGET=flipflop

VPATH = src:assets
LIBT7D ?= libT7D.lib

all:	frame.petscii.data $(TARGET).prg

frame.%.data: %.c
	petsciiconvert --first=0 --last=0 -o $@ $<

$(TARGET).prg:	$(TARGET).raw
	dali --sfx 0x700 -o $@ $^

$(TARGET).raw:	     startup.o muzak.o basic-moiety.o mainloop.o isrs.o display_gamescreen.o game-original.o game-doubleflip.c titlescreen-moiety.o selectscreen.o helpscreen-moiety.o credits-moiety.o credits-moiety-c.o creditsscreen.o
	$(LD) $(LDFLAGS) -o $@ -m $(basename $@).map -Ln $(basename $@).label $^ $(LIBT7D) LAMAlib.lib

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data globals.? temporary.*
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
