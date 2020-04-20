#! /usr/bin/make
##echo $(shell pgrep -f "x64.*-remotemonitor" || x64 -remotemonitor )&

AFLAGS = -DNDEBUG
#AINC = -I includeCC65
OBJS = wavy.o startup.o fillcolram.o

TARGET = wavy

all:	$(TARGET)

%.o:	%.s
	ca65 $(AINC) $(AFLAGS) $+

$(TARGET):	$(OBJS)
	cl65 $(CFLAGS) -v -m $(TARGET).map -Ln $(TARGET).vicelabel -C $(TARGET).cfg $+

clean:
	rm -f *.o $(TARGET).map $(TARGET).vicelabel $(TARGET)


.PHONY:	clean
