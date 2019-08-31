#! /usr/bin/make -f

.PHONY: all
all:	bokanoid-desire.bin.lz4 bokanoid

bokanoid-desire.bin.lz4: bokanoid-desire.bin
	lz4 -z -9 "$+"

bokanoid:	bokanoid.asm lz4.asm
	xa -o $@ bokanoid.asm

.PHONY: clean distclean
clean:
	rm -f bokanoid-desire.bin.lz4 bokanoid
distclean: clean
	rm -f *~
