#! /usr/bin/make -f


.PHONY: all clean

all: simple_example.crt

clean:
	rm *.crt *.bin

simple_example.crt: simple_example.asm
	xa -o simple_example.bin simple_example.asm
	cartconv -i simple_example.bin -p -o simple_example.crt -t normal -n "SIMPLE EXAMPLE"
