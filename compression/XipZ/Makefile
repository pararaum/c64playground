#! /usr/bin/make -f

CXXFLAGS = -DNDEBUG -Wall -O2 -Wextra

VERSION = 0.4.1
#
OBJS = xipz.o cmdline.o qadz.o
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

.PHONY:	all bin clean install pkg

all:	decrunchxipzstub.inc xipz

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

decrunchxipzstub:	decrunchxipzstub.s
	cl65 --asm-include-dir ../../library -C /usr/share/cc65/cfg/c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $+

decrunchxipzstub.inc:	decrunchxipzstub
	xxd -i $+ > $@
	hd $+

decrunchqadzstub.inc:	decrunchqadzstub
	xxd -i $+ > $@
	hd $+

decrunchqadzstub:	decrunchqadzstub.s
	cl65 --asm-include-dir ../../library -C /usr/share/cc65/cfg/c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $+

clean:
	rm -f *.lst *.map *.label *.o *.inc
	rm -f decrunchqadzstub decrunchxipzstub
	rm -f xipz
	rm -rf html

distclean:
	$(MAKE) clean
	rm -f xipz.tbz
	rm -f cmdline.*

xipz:	decrunchxipzstub.inc decrunchqadzstub.inc $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJS)
