#! /usr/bin/make -f
OBJS = hello.o hellotext.o
all:
	@echo make byhand
	@echo make cl65
	@echo make abs

abs:
	ca65 -t c64 absolute.s
	ld65 -v -C absolute.cfg absolute.o
	hd a.out
	da65 a.out

%.o:	%.s
	ca65 -t c64 $+

%.s:	%.c
	ca65 -O -t c64 $+

byhand: hellotext.o hello.o
	ld65 -o hello -t c64 hello.o hellotext.o /usr/share/cc65/lib/c64.lib

cl65:
	cl65 -O -t c64 hello.c hellotext.s

clean:
	rm -f a.out hello *.o
