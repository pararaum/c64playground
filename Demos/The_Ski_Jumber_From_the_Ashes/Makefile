#! /usr/bin/make -f

CC=cl65 -t c64 -I $$PWD
CFLAGS=-Os $(CDEBUG)
AS=ca65 -t c64 -I $$PWD
ASFLAGS=$(ASDEBUG)
LD=cl65 -t c64
LDFLAGS=-C c64-2200.cfg
TASS=64tass
ifdef LIBDIR
 LDFLAGS+=-L $(LIBDIR)
 ASFLAGS+=-I $(LIBDIR)
 CFLAGS+=-I $(LIBDIR)/include
 LIBT7D=$(LIBDIR)/libT7D.lib
 TASSFLAGS=-I$(LIBDIR)
else
 TASSFLAGS=-I/usr/share/cc65
endif
ifdef DEBUG
 CFLAGS += -DDEBUG=$(DEBUG)
 ASFLAGS += -DDEBUG=$(DEBUG)
endif
TARGET=satellite-crash

VPATH = src:assets
LIBT7D ?= libT7D.lib

ZFILES = takeover.Z resurrection.Z slow-start.Z the-jump.Z earth_to_orbit.Z replay.Z crash.Z private.Z

all:	globals.i dload.prg resident \
	from_the_ashes.e000.prg \
	hyperspace.standalone.prg takeover.standalone.prg resurrection.standalone.ts.prg slow-start.standalone.prg the-jump.standalone.prg earth_to_orbit.standalone.prg replay.standalone.prg crash.standalone.prg private.standalone.prg \
	takeover.prg \
	tsjfta.d64

globals.i:	config.ini
	ini2headers.py -CAS config.ini

tsjfta.d64: assets/skijumper.dirart.d64 boot resident $(ZFILES)
	rm -f $@
	cp assets/skijumper.dirart.d64 $@
	cc1541 $(addprefix -m -w , $(filter-out assets/skijumper.dirart.d64,$(^))) $@
	cc1541 -m -f hyperspace.2200 -w hyperspace.2200.prg $@

resident:	resident.o
	$(LD) -C resident.cfg --start-addr 0x800 -m $@.map -o $@ $<  $(LIBT7D) LAMAlib.lib


%.Z: %.2200.prg
	pucrunch -c0 -x 0x2200 $< $@


#%.pu: %.asm
#	$(TASS) $(TASSFLAGS) -l $@.label -o- $^ | pucrunch -x64738 -c0 > $@

%.e000.pu:	%.asm
	64tass -o - -L $@.labels.tmp $^ | pucrunch -x 0xe000 -c0  > $@


# Hyperspace needs extended memory configuration
hyperspace.2200.prg: hyperspace.o
	$(LD) --start-addr 0x2200 -C c64-2200-CC00.cfg -o $@ -Ln $@.label -m $@.map  $(filter %.o, $^) $(LIBT7D) LAMAlib.lib

%.2200.prg: %.o
	$(LD) --start-addr 0x2200 -C c64-2200.cfg -o $@ -Ln $@.label -m $@.map  $(filter %.o, $^) $(LIBT7D) LAMAlib.lib

%.0700.prg: %.o
	$(LD) -C c64-asm.cfg --start-addr 0x700 -o $@ -m $@.map  $(filter %.o, $^) $(LIBT7D) LAMAlib.lib

%.e000.prg: %.petscii.c
	petsciiconvert --generate-code --generate-jumptable $< > temporary.$@.s
	echo 'ANIMATIONSCREEN=$$C000' >> temporary.$@.s
	cl65 -o $@ -C c64-asm.cfg --start-addr 0xe000 temporary.$@.s

%.zx02: %
	zx02 $< $@

temporary.%.petscii.inc: %.petscii.c
	petsciiconvert --generate-code --generate-jumptable $< > $@ 2> $@.imports

temporary.4--10.%.petscii.inc: %.petscii.c
	petsciiconvert --first=4 --last=10 --generate-code --generate-jumptable $< > $@ 2> $@.imports

temporary.%.petscii.bin: %.petscii.c
	petsciiconvert -o $@  $< >$@.out

#

crash.2200.prg: crash.main.o crash.starfield.o crash.o

crash.o: temporary.4--10.jumper_in_orbit_crash.petscii.inc temporary.jumper_in_orbit_crash.petscii.bin

earth_to_orbit.2200.prg: earth_to_orbit.main.o earth_to_orbit.o

earth_to_orbit.o: temporary.earth2orbit.petscii.inc copy_major_tom_to_1000.e000.pu

takeover.2200.prg: resident takeover-palmtree.petscii.o takeover.o

resurrection.2200.prg: resurrection.main.o resurrection.o Orbiter.both.64c.zx02

slow-start.o: sfx.1000.prg.zx02 slow-start.s

slow-start.2200.prg: slow-start.o moiety.frameengine.o

replay.2200.prg: replay.o moiety.frameengine.o

the-jump.2200.prg: the-jump.o ski-jump.petscii.o

# Standalone
%.standalone.prg: %.2200.prg boot.standalone.0700.prg resident
	prepender64 --copy-eor -j 0x0700 -o $@ $^ assets/La_Fusiona_de_Dangera.sid,0x1000,0x7c

%.ts.prg: %.prg
	exomizer sfx systrim -o $@ $<


takeover.standalone.prg: takeover.2200.prg boot.standalone.0700.prg resident

resurrection.standalone.prg: resurrection.2200.prg boot.standalone.0700.prg resident

takeover.prg: takeover.2200.prg boot.takeover.o takeover-palmtree.petscii.o
	$(LD) -C c64-asm.cfg --start-addr 0x4000 -o $@.tmp -m $@.map  $(filter %.o, $^) $(LIBT7D) LAMAlib.lib
	prepender64 --copy-eor -j 0x4000 -o $@ $@.tmp takeover.2200.prg assets/La_Fusiona_de_Dangera.sid,0x1000,0x7c



# Dreamloader
dload.prg: dload.cfg
	cp $< /usr/src/dreamload
	make -C /usr/src/dreamload dload.prg
	cp $(addprefix /usr/src/dreamload/, dload.prg dload-labels.inc) .

boot:	globals.i dload.prg boot.o
	$(LD) -C c64-asm.cfg --start-addr 0x9000 -o $@.tmp -m $@.map boot.o  $(LIBT7D) LAMAlib.lib
	prepender64 -e 0x89 --copy-eor -j 0x9000 -o $@.2.tmp dload.prg $@.tmp
	pucrunch $@.2.tmp $@

run:
	x64 $(TARGET).prg

.phony: clean run
clean:
	rm -f *.o *.raw *.map *.label *.pu *.tmp *.data temporary.* *.zx02
	rm -f $(TARGET) $(TARGET).prg $(TARGET).raw
	rm -f dload.prg dload-labels.inc
	rm -f boot resident  $(ZFILES)
	rm -f *.d64
	rm -f *.prg
	rm -f globals.? globals.sh
