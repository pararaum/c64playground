#! /usr/bin/make -f

CC65ROOT=/usr/share/cc65
CC65ASMINC=$(CC65ROOT)/asminc
CC65LIB=$(CC65ROOT)/lib

CFLAGS = -O -I $(CURDIR)/include -DNDEBUG
CC = cl65

AFLAGS = -DNDEBUG
AINC = -I $(CURDIR)
OBJS = disable_cia_irq.o fill_colour_ram.o crc8.o stackfunctions.o prng.xorshift.o petscii_screencode.o prng.lfsr.o multiply_by_40.o memcpy1K_via_ptr.o memcpy_up.o fill_1000_bytes.o copy_chargen.o takeover_effects/bullets_to_right_bottom.o busywait_frame_pm.o busywait_frame_mp.o spriteX2E-init.o spriteX2E-copy_sprite_shadowregs.o spriteX2E-update_sprite.o bit_to_2power_n_value_table.o sprite_high_bits.o koala-copy-koala_picture-data.o animspriteXORptr.o sprite.set_all_sprites_colour.o spritetext.print_char_to_sprite.o spritetext.print_line_to_sprite.o
COBJS = $(patsubst %.c, %.o, $(wildcard libsrc/*.c))
PROJECT = libT7D

export	CC CFLAGS

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

$(PROJECT).lib:	$(OBJS) $(COBJS)
	ar65 a $@ $(OBJS)
	ar65 a $@ $(COBJS)

clean:
	$(MAKE) -C test clean
	rm -f *.o takeover_effects/*.o libsrc/*.o
	rm -f $(PROJECT).*

distclean:	clean
	rm -f *~

test:
	make -C test

.PHONY:	all clean distclean test
