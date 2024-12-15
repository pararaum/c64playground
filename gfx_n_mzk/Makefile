#! /usr/bin/make -f

TARGETS = cute_cat_ninja cat-ninja secret_ninja_christmas display_multicolour_image display_image

.PHONY:	all clean
all:	$(TARGETS)

clean:
	rm -f $(TARGETS) *.o *.prg

display_multicolour_image: display_multicolour_image.asm
	xa -M -I ../include/ -l /dev/stdout -o $@ $+

display_image: display_image.s
	cl65 --asm-include-dir ../includeCC65/ -C display_image.cfg $+

secret_ninja_christmas: secret_ninja_christmas.s
	cl65 -m /dev/stdout -C secret_ninja_christmas.cfg $+ libT7D.lib
	xipz -j 0x900 -a qadz $@

cat-ninja: cat-ninja.s
	cl65 -m /dev/stdout -C secret_ninja_christmas.cfg $+ libT7D.lib
	pucrunch -x0x900 $@ $@.prg

cute_cat_ninja: cute_cat_ninja.s
	cl65 -m /dev/stdout -C secret_ninja_christmas.cfg $+ libT7D.lib
	pucrunch -x0x900 $@ $@.prg
