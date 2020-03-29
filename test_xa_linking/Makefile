#! /usr/bin/make

OBJS= _stub.o65 file.1.o65 file.2.o65

all:	raw.bin

raw.bin:	linked.o65
	reloc65	-o $@ -X $+

linked.o65:	$(OBJS)
	ldo65 -o $@ $+
	file65 $@

%.o65:	%.asm
	xa -v -R -c -o $@ $+

clean:
	rm -f *.o65 raw.bin
