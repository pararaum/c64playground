#! /usr/bin/make -f

CC=cl65
CFLAGS=-O
AS=ca65
TARGET=mugwump
TARGETSYSTEM=c64
OBJS=$(TARGET).o c64asmcode.o mugwump-muzak.o mugwump-font.o mugwump-titlescreen.o mugwump-credits.o

.PHONY:	all clean distclean doc

all:	$(TARGET).prg astrorescue.prg

$(TARGET): $(OBJS)
	cl65 -C memorymap.cfg -t $(TARGETSYSTEM) -m $(TARGET).map -Ln $(TARGET).label $(CC65FLAGS) $(OBJS) libT7D.lib

%.prg: %
	#pucrunch -x828 $^ $@
	dali -o $@ --sfx 828 $^

%.o: %.c
	$(CC) -c $(CFLAGS) -t $(TARGETSYSTEM) -o $@ $^

%.o: %.s
	$(AS) $(AFLAGS) -t $(TARGETSYSTEM) -o $@ $^

clean:
	rm -f *.o *.map *.label $(TARGET)
	rm -rf $(TARGET).prg $(TARGET) $(OBJS)
	rm -rf astrorescue astrorescue.prg
	rm -rf html
	rm -rf 🖬

distclean:
	$(MAKE) clean
	rm -f *~

doc:
	doxygen

astrorescue: astrorescue.o c64asmcode.o astrorescue-muzak.o astrorescue-font.o astrorescue-titlescreen.o astrorescue-credits.o
	cl65 -C memorymap.cfg -t $(TARGETSYSTEM) -m $@.map -Ln $@.label $(CC65FLAGS) $^ libT7D.lib
