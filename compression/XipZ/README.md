# XipZ #

XipZ is designed for a very specific task:
https://itch.io/jam/the-c64-cassette-50-charity-competition. The
design goal was to have a small stub with a decent compression ration
while meeting the competition rules.

It is another special purpose decruncher for very small games, demos,
etc. It is based on the ideas of XIP by S. Judd and has a cruncher
which uses an algorithm similar to LZ77 compressors.

The program has to be runnable vir `RUN` therefore we needed a basic
header and the maximum memory position to use was $0FFF.

# Usage #

XipZ will use only the memory up to 0x1000 and decompress the file at
the original load address. Interrupts are disabled and BASIC and
KERNAL ROM are still switched on. After decompression a `JMP` to the
load address is performed so make sure that your program starts there
or has a jump prepended.

*Warning!* If you like to return to the OS via `RTS` make sure to pull
the top element of the stack as the decode leaves a garbage byte on
the stack. This was done to save some bytes in the decompressor.

## Simple Usage ##

The XipZ executable is called via:

	xipz [OPTION]… <filename> [<outputfilename>]

It will create a file named like the original file but with an added
".prg". This is the C64 binary which can be loaded with `load
"*",8`. Start this program with `RUN`.

## Advanced Usage ##

XipZ has some advanced features which can be used like writing in a
"raw" version where no decrunching stub is added (in this case the
default extension added is, of course, ".raw"). For further help just
call the executable with the "-h" option switch, like this `xipz -h`.

Here is an excerpt from the command-line help:

	Usage: XipZ [OPTION]...  <filename> [<outputfilename>]

	-h, --help			  Print help and exit
	-V, --version		  Print version and exit
	-r, --raw			  output raw crunched data without header  (default=off)
	-a, --algorithm=ENUM  crunching algorithm to use  (possible values="xipz",
							"qadz" default=`xipz')
	-j, --jump=INT		  address to jump to (-1 = load address)  (default=`-1')

# Building #

## Prerequisites ##

The following tools and packages are needed:

 * g++
 * make
 * cc65
 * xxd, hd
 * libboost-dev
 * gengetopt

## Compilation ##

Usually a `make` should be sufficient to build the executable.

# Algorithms #

## XipZ ##

A very simple algorithm is used.  It has to, to keep the decompression
routine small.  All it does is assign n bits to the most common
values, and eight bits to the rest. Every token is prepended by a bit
indicating if the following is a verbatim byte or a compressed byte.

The decompression algorithm is:

 * read the first bit
 * According to the bit
   - if bit=0, then read next n bits and look up value in table
   - if bit=1, then read next 8 bits store value, increment pointers, and keep going

The decoding stops when the source pointer hits 0x1000.

Have a look at the main_xipz() function.

## qadz ##

This is a LZ77 variant. The stream is scanned for recurring byte
sequences and these are stored as back references. Data is then
classified as either a literal token run or as a back reference
token. These are emitted into the output stream.

The decompression is done as following:

 * Get a single byte from the stream as a signed eight bit integer.
   - If it is zero, the decompression ends
   - If it is greater than zero the following number of bytes are
     copied verbatim into the output.
   - If it is less than zero, it is negated and a further byte is
     read. This last byte will indicate how far to go back and the
     negated byte indicates how many bytes are to be copied to the
     output stream.
 * Advance the corresponding pointers.
 * Rinse and repeat.

This is very similar to the way LZ4 handels the data but instead of
nibbles we use whole bytes as the 6502 architecture is ill equipped to
handle nibbles.

Have a look at the main_qadz() function.

# Maximizing compression #

## XipZ Algorithm ##

The following text was taken mostly verbatim from XIPs manual.

Optimizing a program for xip is not very hard and can easily get you
several dozen more bytes.

The key to maximizing compression is to maximize the frequency of the
most common bytes.  For example, a common byte is $A9 (LDA #).  In
zero page, location $a9 isn't used for anything, so if you choose $a9
instead of, say, $02 or $fe, you will get a lot more occurances of the
byte "$a9" in your program.  Alternatively, if you have a choice between
"bcc" or "bne", opcode bne is $d0 -- and chances are there are plenty
of $d020 and such calls in your code.

The exact formula for data bit count is the sum of:

 * literals * 8
 * n * compressed bytes
 * total bytes (one bit must be prepended)
 
Divide the number by eight to get the number of bytes.

So, the BEST case is having a "top-heavy" program -- just a few byte
values representing most of the program bytes.  So wherever you can choose
a byte -- variables, instructions, lables, whatever -- choose wisely!

When you run the program, it lists the 64 most common bytes and how often
they occur.  This is for the use of you, the programmer, as a tool to see
what you might do to get greater compression -- by switching variable names
around (both zp and absolute), perhaps using different instructions, etc.
Also it's just kind-of interesting :).

The program also lists the compression performance for various values of n,
the number of bits. 

### Some common bytes and corresponding memory locations ###

Remember: you can use zp locations (like $20), absolute locations like $2020,
and combined locations like $204c or whatever.

 * $00	addr	$00	CPU port, do not mess with it.
 * $10	$10	basic flag; most usable
 * $18	clc	$18	basic var (string pointer); most usable
 * $20	jsr	$20	usable (basic var, string descriptors)
 * $30	bmi	$30	usable (basic storage pointer)
 * $38	sec	$38	usable (top of BASIC mem)
 * $4c	jmp	$3c	usable (yet another BASIC pointer/work var)
 * $60	rts	$60	usable, if not using various BASIC routines (work var)
 * $85	sta zp	$85	part of CHRGET; usable if not using/exiting to BASIC
 * $88	dey	$88	same as above
 * $8d	sta abs	$8d	usable zp location -- RND seed
 * $8e	stx abs	$8e	usable -- RND seed
 * $9d	sta $9d	kernal flag; usable
 * $a2	ldx #$a2 jiffy clock; updated by system IRQ; do not use if system IRQ active
 * $a5	lda zp	$a5	usable; tape drive counter
 * $a9	lda #$a9 usable zp location (RS-232 flag)
 * $c8	iny	$c8	usable if not using FFD2, PLOT, etc. (screen routines)
 * $c9	cmp #$c9 same as above
 * $ca	dex	$ca	same
 * $d0	bne, $d0 Flag used by CHRIN (input screen/keyboard); usable i/o	$d0xx
 * $e8	inx	$e8	screen line link table; ok if not using FFD2 etc.

## qadz ##

It seems that using xipz on a data compressed with qadz still shaves
some bytes of. Remember to set the jump address to 2061 (0x80d) so the
the previous decompression stub is called.

# Links #

 * https://csdb.dk/release/?id=6646
 * https://github.com/pararaum/c64playground/tree/xipz/compression/XipZ
