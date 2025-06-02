#! /usr/bin/make -f

CXXFLAGS = -std=c++20 -DNDEBUG -Wall -O2 -Wextra

VERSION = 0.2.0
#
OBJS = cmdline.o prepender64.o
#
PACKAGENAME=prepender64
prefix = $(DESTDIR)/usr/local
INSTALL=/usr/bin/install
TMP=/tmp
exec_prefix = $(prefix)
datarootdir = $(prefix)/share
datadir = $(datarootdir)
bindir = $(exec_prefix)/bin
docdir = $(datadir)/doc/$(PACKAGENAME)
mandir = $(datarootdir)/man
TARGET = $(PACKAGENAME)

.PHONY:	all bin clean install pkg

all:	bin

bin:	logo.80x50.raw vcclogo.prg cmdline.c $(TARGET)

install:	bin doc
	mkdir -p $(bindir)
	mkdir -p $(docdir)
	$(INSTALL) -p -s $(TARGET) $(bindir)
	cp -r README.md html $(docdir)

pkg:	bin doc
	@sed -i 's/version "\(.*\)"/version "$(VERSION)"/' $(PACKAGENAME).ggo
	$(eval PKGTMP := $(shell mktemp -d /tmp/$(PACKAGENAME)-🖬-XXXXXXX))
	mkdir $(PKGTMP)/$(PACKAGENAME)-$(VERSION)
	$(INSTALL) -p -s $(PACKAGENAME) $(PKGTMP)/$(PACKAGENAME)-$(VERSION)
	cp -r html $(PKGTMP)/$(PACKAGENAME)-$(VERSION)
	tar -C $(PKGTMP) -cjf $(PACKAGENAME).tbz $(PACKAGENAME)-$(VERSION)

doc:
	doxygen

cmdline.c: prepender64.ggo
	gengetopt -i $+


logo.80x50.raw: logo.80x50.png
	petscii80x50 $^ > $@

%.prg:	%.petscii.c
	petscii2x.py -f LISTSYS -o $@ $^

%.inc:	%.s
	cl65 -C c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $<
	xxd -i $(basename $<) | tee $@
	cat $@.label | awk '/stub/ { sub(/^./, "STUB$(shell echo $<| sed 's/stub-\([a-zA-Z0-9]\+\).*/\1/'|tr [a-z] [A-Z])", $$3); printf("#define %s 0x%s\n", $$3, $$2);}' | tee -a $@

%.inc:	%.asm
	64tass -o $(basename $<) --vice-labels-numeric -l $@.label -L $@.lst $<
	xxd -i $(basename $<) | tee $@
	cat $@.label | awk '/stub.*offset/ { sub(/^./, "STUB$(shell echo $<| sed 's/stub-\([a-zA-Z0-9]\+\).*/\1/'|tr [a-z] [A-Z])", $$3); printf("#define %s 0x%s\n", $$3, $$2);}' | tee -a $@

clean:
	rm -f $(basename $(wildcard *.inc))
	rm -f *.lst *.map *.label *.o *.inc *.prg
	rm -rf html
	rm -f $(TARGET) cmdline.?
	rm -f $(PACKAGENAME).tbz

distclean:
	$(MAKE) clean
	rm -f cmdline.*

$(TARGET).o: stub-t7dlogo.inc stub-vcclogo.inc stub-autostart326.inc stub-scrambler16.inc stub-copyeor.inc stub-copyeorstack.inc stub-donotspread.inc cmdline.o $(TARGET).cc

$(TARGET): $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(filter %.o, $^)
