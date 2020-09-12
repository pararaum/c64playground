#! /usr/bin/make -f

AFLAGS = -DNDEBUG
AINC = -I ../includeCC65
OBJS = disable_cia_irq.o
PROJECT = libT7D

all:	$(PROJECT).lib

%.o:	%.s
	ca65 $(AINC) $(AFLAGS) $+

$(PROJECT).lib:	$(OBJS)
	ar65 a $@ $+

clean:
	rm -f *.o
	rm -f $(PROJECT).*

distclean:	clean
	rm -f *~

.PHONY:	all clean distclean
