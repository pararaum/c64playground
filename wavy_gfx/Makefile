#! /usr/bin/make -f
##echo $(shell pgrep -f "x64.*-remotemonitor" || x64 -remotemonitor )&

AFLAGS = -DNDEBUG
AINC = -I ../includeCC65
OBJS = wavy.o startup.o fillcolram.o animationfont.o

TARGET = wavy

all:	$(TARGET).prg

%.o:	%.s
	ca65 $(AINC) $(AFLAGS) $+

$(TARGET).prg:	$(TARGET)
	(cat copy_n_uncompress.stub;lz4 -BD -9 < $+) > $@

$(TARGET):	$(OBJS)
	cl65 $(CFLAGS) -v -m $(TARGET).map -Ln $(TARGET).vicelabel -C $(TARGET).cfg $+

clean:
	rm -f *.o $(TARGET).map $(TARGET).vicelabel $(TARGET) $(TARGET).prg


.PHONY:	clean
