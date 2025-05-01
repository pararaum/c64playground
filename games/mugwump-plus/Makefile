#! /usr/bin/make -f

CC=cl65
CFLAGS=-O
AS=ca65
TARGET=mugwump
TARGETSYSTEM=c64
OBJS=$(TARGET).o c64asmcode.o mugwump-lowcode.o mugwump-consts.o

ifdef T7DINCLUDE
CLFLAGS += --asm-include-dir $(T7DINCLUDE)
ASFLAGS += -I $(T7DINCLUDE)
endif

.PHONY:	all clean distclean doc

all:	$(TARGET) $(TARGET)-listart.prg

$(TARGET).prg: mugwump-titlescreen.bin credits-petscii.tmp $(OBJS)
	cl65 -o $@ -t $(TARGETSYSTEM) -m $(TARGET).map -Ln $(TARGET).label $(CC65FLAGS) $(OBJS) libT7D.lib

mugwump-titlescreen.bin: mugwump-petscii-work.c
	petsciiconvert -s 0x400 --first=0 --last=0 -o $@ --separate-frame $<
	mv $@.0000 $@

$(TARGET)-listart.prg: $(TARGET).prg
	petscii2x.py --append-prg $(TARGET).prg -o $@ mugwump-listart.c -f LISTSYS

%: %.prg
	subsizer -o $@ -x -Xjmp=2061 $^
#pucrunch $^ $@
#nucrunch -o mugwump.nu -x mugwump.prg
#dali --sfx 2061 $^

%.o: %.c
	$(CC) -c $(CFLAGS) -t $(TARGETSYSTEM) -o $@ $^

%.o: %.s
	$(AS) $(AFLAGS) -t $(TARGETSYSTEM) -o $@ $^

%-petscii.tmp: %-petscii.c
	petsciiconvert -s 0x400 -o $@ < $<

clean:
	rm -f *.tmp *.o *.map *.label $(TARGET)
	rm -rf $(TARGET).prg $(TARGET) $(OBJS)
	rm -rf html
	rm -rf 🖬

distclean:
	$(MAKE) clean
	rm -f *~

doc:
	doxygen
