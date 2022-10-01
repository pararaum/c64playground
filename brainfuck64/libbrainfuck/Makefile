#! /usr/bin/make -f

CC=cl65
CFLAGS=-O -I $(LIBDIR)/include $(CDEBUG)
AS=ca65
ASFLAGS=-t c64 -I $(LIBDIR) -I $(LIBDIR) $(ASDEBUG)
LD=cl65
LDFLAGS=-L $(LIBDIR)
AR=ar65
LIBDIR=../../library
X64=x64
X64FLAGS=+truedrive -device8 1 -fs8 . -autostartprgmode 1
CRUNCHER=pucrunch
CRUNCHERFLAGS=

PROJECT = brainfuck

.PHONY:	all test run clean debug

all:	lib

test:

lib:	$(PROJECT).lib

clean:
	rm -f $(PROJECT).lib *.o *.map *.labels

distclean:	clean
	rm -f *~

$(PROJECT).lib: interpreter.o
	ar65 a $@ $+

