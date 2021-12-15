
CFLAGS += -t sim6502
ASFLAGS += -t sim6502 -I ../

.PHONY: test
test:	$(patsubst %.c, %.run, $(wildcard test_*.c)) $(patsubst %.s, %.run, $(wildcard test_*.s))
	for x in *.run; do printf "\033[42m%60s\033[0m\n" $$x; sim65 $$x; done

%.run: %.o
	cl65 -t sim6502 -o $@ $+ ../libT7D.lib

clean:
	rm -f *.o *.run
