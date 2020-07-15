#! /usr/bin/make
##echo $(shell pgrep -f "x64.*-remotemonitor" || x64 -remotemonitor )&

AFLAGS = -DNDEBUG
AINC = -I includeCC65
PROJECT = text_displayer
OBJS = vicmoiety.o irqmoiety.o
LINKCONF = muzak.cfg

all:	$(PROJECT)

%.o:	%.s
	ca65 $(AINC) $(AFLAGS) $+

$(PROJECT).lib:	$(OBJS)
	ar65 a $@ $+

$(PROJECT):	$(PROJECT).o muzak.o $(PROJECT).lib
	ld65 -o $(PROJECT) -v -m $(PROJECT).map -Ln $(PROJECT).vicelabel -C $(LINKCONF) $+

clean:
	rm -f $(PROJECT) $(PROJECT).prg $(PROJECT).map $(PROJECT).vicelabel *.o
	rm -f $(PROJECT).lib

.PHONY:	all clean
