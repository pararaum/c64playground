#! /usr/bin/make -f

TARGETS = display_multicolour_image

.PHONY:	all clean
all:	$(TARGETS)

clean:
	rm $(TARGETS)

display_multicolour_image: display_multicolour_image.asm
	xa -M -I ../include/ -l /dev/stdout -o $@ $+
