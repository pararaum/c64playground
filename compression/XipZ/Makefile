#! /usr/bin/make -f

CXXFLAGS = -DNDEBUG -Wall -O2 -Wextra -std=c++23

ifeq ($(OS),Windows_NT)
  HD = xxd
else
  HD = hd
endif

VERSION = 0.6.1
#
OBJS = cmdline.o xipz.o compression.o qadz.o lzp.o rle.o read_data.o write_compressed_data.o
#
PACKAGENAME=xipz
prefix = $(DESTDIR)/usr/local
INSTALL=/usr/bin/install
TMP=/tmp
exec_prefix = $(prefix)
datarootdir = $(prefix)/share
datadir = $(datarootdir)
bindir = $(exec_prefix)/bin
docdir = $(datadir)/doc/$(PACKAGENAME)
mandir = $(datarootdir)/man

STUBINCS = decrunchlzpstub.inc decrunchlzp2stub.inc decrunchqadzstub.inc decrunchxipzstub.inc

.PHONY:	all bin clean install pkg

all:	$(STUBINCS) xipz

bin:	xipz

install:	bin doc
	mkdir -p $(bindir)
	mkdir -p $(docdir)
	$(INSTALL) -p -s xipz $(bindir)
	cp -r README.md html $(docdir)

pkg:	bin doc
	@sed -i 's/version "\(.*\)"/version "$(VERSION)"/' command_line.ggo
	$(eval PKGTMP := $(shell mktemp -d /tmp/xipz-🖬-XXXXXXX))
	mkdir $(PKGTMP)/xipz-$(VERSION)
	$(INSTALL) -p -s xipz $(PKGTMP)/xipz-$(VERSION)
	cp -r html $(PKGTMP)/xipz-$(VERSION)
	tar -C $(PKGTMP) -cjf xipz.tbz xipz-$(VERSION)

doc:
	doxygen

cmdline.c: command_line.ggo
	gengetopt -i $+

%.prg:	%.s
	cl65 -o $@ --asm-include-dir ../../library -C c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $+

%.inc:	%.prg
	xxd -i $< > $@
	$(HD) $<

clean:
	rm -f *.lst *.map *.label *.o *.inc
	rm -f cmdline.?
	rm -f decrunch*stub.*
	rm -f xipz
	rm -rf html

distclean:
	$(MAKE) clean
	rm -f xipz.tbz
	rm -f cmdline.*

xipz:	decrunchxipzstub.inc decrunchqadzstub.inc $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJS)
