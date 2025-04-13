#! /usr/bin/make -f

CC=cl65 -t c64
CFLAGS=-Os $(CDEBUG)
AS=ca65 -t c64
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
TARGET=like_black

VPATH = src:assets
LIBT7D ?= libT7D.lib

all:	$(TARGET).prg

$(TARGET).prg:	$(TARGET)
	exomizer sfx 2049 -o $@ $^

$(TARGET):	    $(TARGET).o city_scene.petscii.o moiety.fld.o
	$(LD) $(LDFLAGS) -o $@ -m $(basename $@).map -Ln $(basename $@).label $^ $(LIBT7D) LAMAlib.lib

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data temporary.*
	rm -f ski_jump_frames.tmp
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
