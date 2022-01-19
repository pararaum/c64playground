CC=cl65
CFLAGS=-Os -I ../../library/include
AS=ca65
ASFLAGS=-I ../../library $(ASDEBUG)
LD=cl65
LDFLAGS=-L ../../library

OBJS=friggineditor.o put_char_in_logical_line.o clear_logical_line.o diskmoiety.o data-structures.o allocate_line.o

.PHONY:	all test run clean

all:	friggineditor

test:	

run:	friggineditor
	x64 +truedrive -autostartprgmode 0 friggineditor

clean:
	rm -f friggineditor *.o *.map *.labels

friggineditor: $(OBJS)
	$(LD) -m $@.map -Ln $@.labels $(LDFLAGS) -o $@ $+ libT7D.lib LAMAlib.lib
