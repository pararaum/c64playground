#! /usr/bin/make -f

AS=ca65
LD=ld65
CC=cl65
CFLAGS=-C memmap.cfg
LDFLAGS=-C memmap.cfg -L../library
ASFLAGS=-I../library -I../includeCC65

TARGETS=julia.prg bananas-01.prg
LIBOBJS=$(addsuffix .o, $(basename $(wildcard lib/*.s)))
OBJS=main.o $(LIBOBJS)

ifdef DEBUG
ASFLAGS += -DDEBUG
endif

.PHONY:	all bin prg run
all:	bin

clean:
	rm -f $(TARGET) *.bin *.o lib/*.o *.prg *.map *.label *.lib a.out

sidplay.lib:	$(LIBOBJS)
	ar65 r $@ $^

bin:	$(OBJS) sidplay.lib $(TARGETS)

julia.bin:	main.o default_screen_simple.o sidplay.lib julia.o
	$(LD) $(LDFLAGS) -m $@.map -Ln $@.label -o $@ $+ libT7D.lib
	@sed '/^Segment list/,/^$$/p' -n $@.map

bananas-01.bin:	main.o default_screen_simple.o sidplay.lib bananas-01.o
	$(LD) $(LDFLAGS) -m $@.map -Ln $@.label -o $@ $+ libT7D.lib

Manual_Vision.bin:	main.o default_screen_simple.o sidplay.lib Manual_Vision.o
	$(LD) $(LDFLAGS) -m $@.map -Ln $@.label -o $@ $+ libT7D.lib

run:	$(TARGETS)
	x64 -keybuf "sys828:\n" julia.bin

%.prg:	%.bin
	./pucrunch -x 0x33c $+ $@
	@#./exomizer sfx 0x33c  -s 'lda #0 sta 53265' -x3 -o $@ $+

#$(LD) $+
