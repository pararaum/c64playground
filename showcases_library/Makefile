#! /usr/bin/make -f

AS=ca65
LD=ld65
CC=cl65
CFLAGS=
LDFLAGS=-L../library
ASFLAGS=-I../library -I../includeCC65

TARGETS=takeover_bulleteffect.prg

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY: all clean

all:	$(TARGETS)

clean:
	rm -f *.o *.labels *.map
	rm -f $(TARGETS)

takeover_bulleteffect.prg: takeover_bulleteffect.o
	$(LD) $(LDFLAGS) -C takeover_bulleteffect.cfg -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib

