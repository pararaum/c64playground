
CFLAGS += -t sim6502

.PHONY: test
test:	$(patsubst %.c, %.run, $(wildcard test_*.c))
	for x in *.run; do sim65 $$x; done

%.run: %.o
	cl65 -t sim6502 -o $@ $+ ../libT7D.lib

clean:
	rm -f *.o *.run
