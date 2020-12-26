#! /usr/bin/make -f

CXXFLAGS = -DNDEBUG -Wall -O2 -Wextra

#
OBJS = xipz.o
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
	$(eval PKGTMP := $(shell mktemp -d /tmp/xipz-🖬-XXXXXXX))
	echo $(PKGTMP)
	$(INSTALL) -p -s xipz $(PKGTMP)
	cp -r html $(PKGTMP)
	tar -C $(PKGTMP) -cjf xipz.tbz html xipz

doc:
	doxygen

decrunchxipzstub:	decrunchxipzstub.s
	cl65 --asm-include-dir ../../includeCC65/ -C /usr/share/cc65/cfg/c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $+

decrunchxipzstub.inc:	decrunchxipzstub
	xxd -i $+ > $@
	hd $+

clean:
	rm -f *.lst *.map *.label *.o
	rm -f decrunchxipzstub decrunchxipzstub.inc
	rm -f xipz
	rm -rf html

distclean:
	$(MAKE) clean
	rm -f xipz.tbz

xipz:	decrunchxipzstub.inc $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJS)
