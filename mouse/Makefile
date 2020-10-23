#! /usr/bin/make -f

TARGET = mouse
EXECADDR = 49152
RUNCMD = "sys$(EXECADDR)\\n"

AS = ca65
LD = cl65
AFLAGS = -DNDEBUG
AINC = -I ../includeCC65 -I ../library
OBJS = $(TARGET).o startup.o interpreter_moiety.o
LIBPATH = --lib-path ../library
LIB = libT7D.lib

all:	$(TARGET) $(TARGET),prg butterflyfly

%.o:	%.s
	$(AS) $(AINC) $(AFLAGS) $+

$(TARGET),prg:	$(TARGET)
	#(cat ../coding/startup/copy_n_uncompress.stub; lz4 -BD -9 < $+) > $@
	pucrunch -x $(EXECADDR) $+ $@
	#./exomizer sfx $(EXECADDR) -n -o $@ $+

$(TARGET):	$(OBJS)
	$(LD) $(CFLAGS) $(LIBPATH) -v -m $(TARGET).map -Ln $(TARGET).label -C $(TARGET).cfg $+ $(LIB)

butterflyfly:	butterflyfly.o
	$(LD) $(CFLAGS) $(LIBPATH) -v -m butterflyfly.map -Ln butterflyfly.label -C butterflyfly.cfg $+ $(LIB)

clean:
	rm -f *.o
	rm -f $(TARGET) $(TARGET).map $(TARGET).label $(TARGET),prg
	rm -f butterflyfly butterflyfly.map butterflyfly.label butterflyfly,prg

run:	$(TARGET)
	x64 -controlport1device 3 -autostartprgmode 1 -keybuf "$(RUNCMD)" $(TARGET)

runbutterflyfly:	butterflyfly
	x64 -controlport1device 3 -autostartprgmode 1 -keybuf "sys32777\\n" $+

.PHONY:	all clean run runbutterflyfly
