#! /usr/bin/make -f

SRC = src
BUILD = build

CXXFLAGS = -I$(BUILD) -DNDEBUG -Wall -O2 -Wextra -std=c++23

ifeq ($(OS),Windows_NT)
  HD = xxd
else
  HD = hd
endif

VERSION = 1.0.0
#
OBJS = $(addprefix $(BUILD)/,cmdline.o xipz.o compression.o qadz.o ratz.o lzp.o rle.o bpe.o squz.o lzw.o lzrc.o lzh16.o timecrunch.o tc.o read_data.o write_compressed_data.o)
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

STUBINCS = $(addprefix $(BUILD)/,decrunchlzpstub.inc decrunchlzp2stub.inc decrunchlzp3stub.inc decrunchlzp4stub.inc decrunchlzp5stub.inc decrunchtcstub.inc decrunchqadzstub.inc decrunchratzstub.inc decrunchrlestub.inc decrunchbpestub.inc decrunchsquzstub.inc decrunchlzwstub.inc decrunchlzrcstub.inc decrunchlzh16stub.inc decrunchxipzstub.inc)

.PHONY:	all bin clean install pkg

all:	$(STUBINCS) xipz

# The compressors embed their stubs.
$(BUILD)/lzp.o:	$(addprefix $(BUILD)/,decrunchlzpstub.inc decrunchlzp2stub.inc decrunchlzp3stub.inc decrunchlzp4stub.inc decrunchlzp5stub.inc)

$(BUILD)/tc.o:	$(BUILD)/decrunchtcstub.inc

bin:	xipz

install:	bin doc
	mkdir -p $(bindir)
	mkdir -p $(docdir)
	$(INSTALL) -p -s xipz $(bindir)
	cp -r README.md html $(docdir)

pkg:	bin doc
	@sed -i 's/version "\(.*\)"/version "$(VERSION)"/' $(SRC)/command_line.ggo
	$(eval PKGTMP := $(shell mktemp -d /tmp/xipz-🖬-XXXXXXX))
	mkdir $(PKGTMP)/xipz-$(VERSION)
	$(INSTALL) -p -s xipz $(PKGTMP)/xipz-$(VERSION)
	cp -r html $(PKGTMP)/xipz-$(VERSION)
	tar -C $(PKGTMP) -cjf xipz.tbz xipz-$(VERSION)

doc:
	doxygen

$(BUILD):
	mkdir -p $@

$(BUILD)/cmdline.c $(BUILD)/cmdline.h: $(SRC)/command_line.ggo | $(BUILD)
	gengetopt -i $< --output-dir=$(BUILD)

$(BUILD)/%.prg:	$(SRC)/%.s | $(BUILD)
	cl65 -o $@ --asm-include-dir ../../library -C c64-asm.cfg  -Ln $@.label -m $@.map --listing $@.lst $<

# xxd names the array after the input path, so run it from inside $(BUILD).
$(BUILD)/%.inc:	$(BUILD)/%.prg
	cd $(BUILD) && xxd -i $(notdir $<) > $(notdir $@)
	$(HD) $<

$(BUILD)/%.o:	$(SRC)/%.cc | $(BUILD)/cmdline.h
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(BUILD)/cmdline.o:	$(BUILD)/cmdline.c
	$(CC) -c -o $@ $<

clean:
	rm -rf $(BUILD)
	rm -f xipz
	rm -rf html

distclean:
	$(MAKE) clean
	rm -f xipz.tbz

xipz:	$(STUBINCS) $(OBJS)
	g++ $(CXXFLAGS) $(LDFLAGS) -o $@ $(OBJS)
