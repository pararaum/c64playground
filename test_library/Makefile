
LIBDIR=../library

BIN=test_yielding

all: $(BIN)

test_yielding: test_yielding.s
	cl65 -C memorymap.cfg -t c64 --asm-include-dir $(LIBDIR) $^ $(LIBDIR)/libT7D.lib

.PHONY:	clean
clean:
	rm -f $(BIN) *.o
