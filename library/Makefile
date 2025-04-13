#! /usr/bin/make -f

CC65ROOT=$(DESTDIR)/usr/share/cc65
CC65ASMINC=$(CC65ROOT)/asminc
CC65INCLUDE=$(CC65ROOT)/include
CC65LIB=$(CC65ROOT)/lib

CFLAGS = -O -I $(CURDIR)/include -DNDEBUG
CC = cl65
AS = ca65

AFLAGS = -DNDEBUG
AINC = -I $(CURDIR)
OBJS = disable_cia_irq.o fill_colour_ram.o crc8.o stackfunctions.o prng.xorshift.o petscii_screencode.o prng.lfsr.o multiply_by_40.o memcpy1K_via_ptr.o memcpy_up.o fill_1000_bytes.o copy_chargen.o spriteX2E-init.o spriteX2E-copy_sprite_shadowregs.o spriteX2E-update_sprite.o bit_to_2power_n_value_table.o sprite_high_bits.o koala-copy-koala_picture-data.o animspriteXORptr.o sprite.set_all_sprites_colour.o spritetext.print_char_to_sprite.o spritetext.print_line_to_sprite.o memcpy_strided.o \
	sweet16.default.o \
	busywait_frame_pm.o busywait_frame_mp.o busywait_frame_0.o \
	yielding.o \
	run_function_list.o \
	spritepad-copy_spad_data.o \
	t7d/takeover_effects/dissolve_left2right.o t7d/takeover_effects/curtain_left2right.o t7d/takeover_effects/water_rising.o t7d/takeover_effects/bullets_to_right_bottom.o \
	theatre.until_frame.o theatre.o theatre.copy_petscii_frame.o theatre.sprite_animation.o theatre.textfunctions.o theatre.wait_frames.o draw_frame.o window_chrout.o window_parameters.o theatre.theatre_copy_compressed_frame.o theatre.theatre_switch_to_windowed.o \
	pudecrunch.o zx02decrunch.o \
	output_long_string.o \
	output_string_deluxe.o \
	increment_and_load_ptr1.o \
	clear_memory.o clear_memory_progmem.o \
	memsetAX.o \
	copy_memory_downwards_ip.o memcpy_down.o \
	spritecanvas3X4.o \
	petsciicopyframe.o \
	vic-set_chargen_memory.o vic-set_screen_memory.o

OBJS += $(patsubst %.s, %.o, $(wildcard fsvscroll.*.s))
COBJS = $(patsubst %.c, %.o, $(wildcard libsrc/*.c))
PROJECT = libT7D

export	CC CFLAGS AS AFLAGS

all:	$(PROJECT).lib

install: all
	mkdir -p $(CC65ASMINC)/t7d
	mkdir -p $(CC65INCLUDE)/t7d
	cp -r t7d/* $(CC65ASMINC)/t7d
	cp -r include/t7d/* $(CC65INCLUDE)/t7d
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
	rm -f *.o t7d/takeover_effects/*.o libsrc/*.o
	rm -f $(PROJECT).lib

distclean:	clean
	rm -f *~

test:
	make -C test

.PHONY:	all clean distclean test
