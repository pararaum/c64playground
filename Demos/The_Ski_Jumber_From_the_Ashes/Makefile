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
TARGET=satellite-crash

VPATH = src:assets
LIBT7D ?= libT7D.lib

all:	anim.ashes.raw $(TARGET).prg

anim.ashes.raw: from_the_ashes.c
	petsciiconvert --generate-code --generate-jumptable --last=29 $< > temporary.$@.s
	echo 'ANIMATIONSCREEN=$$C000' >> temporary.$@.s
	cl65 -o $@ -C c64-asm.cfg --start-addr 0xe000 temporary.anim.ashes.raw.s


$(TARGET).prg:	$(TARGET).raw
	exomizer sfx systrim -o $@ $^

$(TARGET).raw:	     $(TARGET).o startup.o moiety.resurrection.o
	$(LD) $(LDFLAGS) -o $@.tmp -m $(basename $@).map -Ln $(basename $@).label $^ $(LIBT7D) LAMAlib.lib
	prepender64 --copy-eor -o $@ $@.tmp anim.ashes.raw

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data temporary.*
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
