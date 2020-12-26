#! /usr/bin/make -f

CXXFLAGS = -DNDEBUG -Wall -O2 -Wextra

#
OBJS = xipz.o

.PHONY:	all clean doc
all:	decrunchxipzstub.inc xipz

doc:
	doxygen

decrunchxipzstub:	decrunchxipzstub.s
	cl65 --asm-include-dir ../../includeCC65/ -C /usr/share/cc65/cfg/c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $+

decrunchxipzstub.inc:	decrunchxipzstub
	xxd -i $+ > $@
	hd $+

clean:
	rm -f *.lst *.map *.label *.o
	rm -f decrunchxipzstub decrunchxipzstub.inc
	rm -f xipz
	rm -rf html

xipz:	decrunchxipzstub.inc $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJS)
