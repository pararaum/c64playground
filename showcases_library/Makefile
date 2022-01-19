#! /usr/bin/make -f

AS=ca65
LD=ld65
CC=cl65
CFLAGS=
LDFLAGS=-L../library
ASFLAGS=-I../library -I../includeCC65

TARGETS=takeover_bulleteffect.prg takeover_dissolve_left2right.prg takeover_curtain_left2right.prg

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY: all clean

all:	$(TARGETS)

clean:
	rm -f *.o *.labels *.map
	rm -f $(TARGETS)

%.prg: %.o
	$(LD) $(LDFLAGS) -C takeover_default.cfg -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib

