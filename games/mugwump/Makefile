#! /usr/bin/make -f

CFLAGS=-Wall -O2 -DNDEBUG
LDFLAGS=-lm
CC65FLAGS=-O
TARGET=mugwump
OBJS=$(TARGET).o xorshiftprng.o

.PHONY:	all c64 sim bin clean
all:	bin c64 sim

sim:
	mkdir -p sim
	cp -al *.c Makefile sim
	make -C sim cc65intern TARGETSYSTEM=sim6502

c64:
	mkdir -p c64
	cp -al *.c Makefile c64
	make -C c64 cc65intern TARGETSYSTEM=c64

cc65intern:
	cl65 -t $(TARGETSYSTEM) -m $(TARGET).map -Ln $(TARGET).label $(CC65FLAGS) $(TARGET).c


bin:	$(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $+ $(LDFLAGS)

clean:
	rm -f *.o *.map *.label $(TARGET)
	rm -rf c64
	rm -rf sim
	rm -rf html
	rm -rf 🖬

distclean:
	$(MAKE) clean
	rm -f *~

check:	xorshiftprng.o tests.o
	gcc -o 🖬 $+ -l cunit
	./🖬
