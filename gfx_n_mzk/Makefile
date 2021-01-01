#! /usr/bin/make -f

TARGETS = display_multicolour_image display_image

.PHONY:	all clean
all:	$(TARGETS)

clean:
	rm -f $(TARGETS) *.o

display_multicolour_image: display_multicolour_image.asm
	xa -M -I ../include/ -l /dev/stdout -o $@ $+

display_image: display_image.s
	cl65 --asm-include-dir ../includeCC65/ -C display_image.cfg $+
