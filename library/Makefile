#! /usr/bin/make -f

CC65ROOT=/usr/share/cc65
CC65ASMINC=$(CC65ROOT)/asminc
CC65LIB=$(CC65ROOT)/lib

AFLAGS = -DNDEBUG
AINC = -I ../includeCC65
OBJS = disable_cia_irq.o fill_colour_ram.o crc8.o stackfunctions.o prng.xorshift.o petscii_screencode.o prng.lfsr.o multiply_by_40.o memcpy1K_via_ptr.o memcpy_up.o fill_1000_bytes.o
PROJECT = libT7D

all:	$(PROJECT).lib

install: all
	mkdir -p $(CC65ASMINC)/t7d
	cp *.i $(CC65ASMINC)/t7d
	cp $(PROJECT).lib $(CC65LIB)

uninstall:
	rm -rf $(CC65ASMINC)/t7d
	rm -f $(PROJECT).lib

%.o:	%.s
	ca65 $(AINC) $(AFLAGS) $+

$(PROJECT).lib:	$(OBJS)
	ar65 a $@ $+

clean:
	rm -f *.o
	rm -f $(PROJECT).*

distclean:	clean
	rm -f *~

.PHONY:	all clean distclean
