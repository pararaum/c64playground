#! /usr/bin/make -f

AS=ca65
LD=ld65
CC=cl65
CFLAGS=-C memmap.cfg
LDFLAGS=-C memmap.cfg -L../library
ASFLAGS=-I../library -I../includeCC65

TARGETS=julia.prg bananas-01.prg
OBJS=main.o

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY:	all bin prg run
all:	bin

clean:
	rm -f $(TARGET) *.bin *.o *.prg *.map *.labels a.out

bin:	main.o $(TARGETS)

julia.bin:	main.o default_screen_simple.o julia.o
	$(LD) $(LDFLAGS) -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib
	@sed '/^Segment list/,/^$$/p' -n $@.map

bananas-01.bin:	main.o default_screen_simple.o bananas-01.o
	$(LD) $(LDFLAGS) -m $@.map -Ln $@.labels -o $@ $+ libT7D.lib

run:	$(TARGETS)
	x64 -keybuf "sys828:\n" julia.bin

%.prg:	%.bin
	./pucrunch -x 0x33c $+ $@
	@#./exomizer sfx 0x33c  -s 'lda #0 sta 53265' -x3 -o $@ $+

#$(LD) $+
