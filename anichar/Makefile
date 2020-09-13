#! /usr/bin/make -f

TARGET = anichar

AS = ca65
LD = cl65
AFLAGS = -DNDEBUG
AINC = -I ../includeCC65 -I ../library
OBJS = $(TARGET).o startup.o irqmoiety.o muzakmoiety.o
LIBPATH = --lib-path ../library
LIB = libT7D.lib

all:	$(TARGET),prg

%.o:	%.s
	$(AS) $(AINC) $(AFLAGS) $+

$(TARGET),prg:	$(TARGET)
	(cat ../coding/startup/copy_n_uncompress.stub; lz4 -BD -9 < $+) > $@

$(TARGET):	$(OBJS)
	$(LD) $(CFLAGS) $(LIBPATH) -v -m $(TARGET).map -Ln $(TARGET).label -C $(TARGET).cfg $+ $(LIB)

clean:
	rm -f *.o $(TARGET) $(TARGET).map $(TARGET).label $(TARGET),prg

.PHONY:	all clean
