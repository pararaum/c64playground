# XipZ #

XipZ is designed for a very specific task:
https://itch.io/jam/the-c64-cassette-50-charity-competition. The
design goal was to have a small stub with a decent compression ration
while meeting the competition rules.

It is another special purpose decruncher for very small games, demos,
etc. It is based on the ideas of XIP by S. Judd and has two crunchers
which uses an algorithm similar to LZ77 compressors.

The program has to be runnable vir `RUN` therefore we needed a basic
header and the maximum memory position to use in the competition was
$0FFF. The program was later extended to handle larger files.

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
default extension added is, of course, ".raw"). The raw data of xipz
starts with the number of bits n (one byte) and the table of 2^n
bytes, followed by the bit stream. For further help just
call the executable with the "-h" option switch, like this `xipz -h`.

Here is an excerpt from the command-line help:

	Usage: XipZ [OPTION]... <filename> [<outputfilename>]

	  -h, --help            Print help and exit
	  -V, --version         Print version and exit
	  -r, --raw             output raw crunched data without header  (default=off)
	  -a, --algorithm=ENUM  crunching algorithm to use  (possible values="xipz",
	                          "qadz", "ratz", "lzp", "lzp2", "lzp3", "lzp4", "lzp5",
	                          "tc", "rle", "bpe", "squz", "lzw", "lzrc", "lzh16"
	                          default=`xipz')
	  -j, --jump=INT        address to jump to (-1 = load address)  (default=`-1')
	  -p, --page=INT        maximum page to use +1  (default=`0xA0')
	  -d, --data            input is raw data without a load address  (default=off)
	  -v, --verbose         more verbose output, if available  (default=off)
	      --debug-json=LIST debug: crunch the raw data (see -d, no stub) with the
	                          comma separated list of algorithms or "all" and
	                          print a JSON object of algorithm and base64 data


For comparing the algorithms there is `--debug-json=LIST`, with a comma
separated list of algorithms or `all`. The input is crunched with each of
them like with `-r` (no stub, the raw format of the algorithm) and a JSON
object is printed to stdout, which maps the name of the algorithm to the
base64 encoded data, e.g. `xipz -d --debug-json=lzp2,lzp3 data.bin`. This
is mostly useful for raw data (`-d`), otherwise the load address is cut
off. The chatter of the algorithms goes to stderr. An algorithm that
cannot handle the data (for example because it would overwrite its own
tables) gets the value `null`, a message on stderr and the exit status is
non-zero.

To get a table of the sizes `jq` (1.6 or newer) can decode the data, the
algorithms that failed are shown as `failed`:

	xipz -d --debug-json=all data.bin 2>/dev/null \
	  | jq -r 'to_entries[] | "\(.key)\t\(if .value then (.value|@base64d|length) else "failed" end)"' \
	  | column -t

Append `| sort -k2 -n` to sort by size. Use `jq 'map_values(if . then
(@base64d|length) else null end)'` instead to get a JSON object of names
and lengths.

Remember that the KERNAL and the BASIC ROMs are still memory
mapped. So using a page above 0xA1 makes no sense.

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

The decoding stops when the source pointer hi-byte hits the 
predefined page.

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

## ratz ##

The successor of qadz, again a byte aligned LZ77 variant without bit
reading or tables, but with a window of 64 KB and a cheaper way to
repeat the last offset. Every token starts with one control byte, the
stream ends with the token `$00`:

| Control byte | Token              | Bytes after it     | Length          |
|--------------|--------------------|--------------------|-----------------|
| `$00`        | end of stream      |                    |                 |
| `$01-$7F`    | literal run        | b raw bytes        | b (1-127)       |
| `$80-$9F`    | match, last offset |                    | b-$80+1 (1-32)  |
| `$A0-$DF`    | match, near        | offset-1 (1 byte)  | b-$A0+2 (2-65)  |
| `$E0-$FF`    | match, far         | offset-1 (lo, hi)  | b-$E0+3 (3-34)  |

Matches are copied forwards, so an offset smaller than the length
repeats data like a run-length encoding. The offset of the last match
of any kind can be reused with a single byte. The parser is a beam
search (see `src/ratz.cc`) and finds a near optimal parse; the compressed
data is decoded again after crunching and compared with the input.

Compared with qadz the data is 0-9% smaller on small C64 programs and
much smaller on larger data (for example 6502 code or text). The
decoder is about as fast as the qadz decoder. The decoding stops at the
end token, not when the source pointer reaches a page. The page option
only sets where the compressed data is moved to before decrunching.

Have a look at `crunch_ratz()` and `src/decrunchratzstub.s`.

## rle ##

A very simple run-length encoding without a marker byte. If two
consecutive bytes are the same, the next byte is a count of further
copies (1-255), so `aa aa 05` is six times `$aa`. A count of zero ends
the stream. Everything else is copied as is. The decoder is tiny, see
`src/decrunchrlestub.s`.

## bpe ##

Byte pair encoding (Gage) with one global table. The most frequent
pair of tokens is replaced by an unused byte value again and again
until a pair no longer pays for its 2 byte table entry. The stream is
`ESC`, `END`, the pair table, the tokens and finally `ESC END`. The
pair table is 32 bitmap bytes (bit b of byte i marks the value 8*i+b
as a pair code), each directly followed by the (left, right) bytes of
the codes it marks. A token which is not a pair code stands for
itself. `ESC` followed by a byte writes this byte raw, this is needed
for `ESC` itself and for byte values which had to be given up as pair
codes (the rarest ones, if the data uses all 256 values).

The decoder (about 120 bytes) is copied to the stack page ($0100) and
expands the tokens with the hardware stack. The global tables are
built in the text screen at $0400-$05FF. Therefore the decrunched
data must not overlap $0400-$05FF, xipz refuses such files. The
nesting depth of a pair is limited to 64 so the stack is sufficient.
The decoder is slow (it is a byte-by-byte table walk) but extremely
small; ratios are between rle and ratz on text-like data. See
`src/bpe.cc` and `src/decrunchbpestub.s`; the raw format is decoded by
`library/t7d/compression/xipz-bpe.decrunch.inc`.

## squz ##

Byte pair encoding combined with an adaptive binary range coder; this
is the "maximum compression" mode, it is slower but smaller than
bpe. The data is first reduced with up to 31 byte pairs (the pair
tokens may be nested, up to eight levels deep). The tokens are then
coded bit by bit with an arithmetic coder, there is no table of code
lengths, the model learns while decoding. The probability of a bit is
selected by the position inside the current 6502 instruction (opcode,
first operand or second operand byte), a context which is derived by
the decoder from the bytes it has written. This is what makes squz good at 6502
code and music (SID) data. The format is described in `src/squz.hh`.

Decoding is slow, roughly 2600 cycles per output byte, so expect around half
a minute for a 10 KB program. The decoder (about 400 bytes) is
distributed over the bottom of the stack page ($0100-$01CA) and the
cassette buffer ($0334-$03FE), the adaptive probabilities and the
dictionary use the text screen ($0400-$07DF), so the program must not
load into $0100-$07FF. Compared with bpe the compressed data is about 12 % smaller and about 5 %
smaller than with ratz (measured on SID tunes, the C64 ROMs and some
programs). The stub is larger, 481 bytes against 179 bytes for bpe, so
squz only pays off for data of more than about 3 KB; on small data bpe
or ratz are still better. See `src/squz.cc`, `src/decrunchsquzstub.s` and for the
raw format `library/t7d/compression/xipz-squz.decrunch.inc` (which needs 13
bytes of zero page and $3E0 bytes of tables, but no special memory
layout).

## lzw ##

LZW with nine bit codes: 256 literals, 254 dictionary entries, a code to
clear the dictionary and an end code. Instead of (prefix, byte) pairs a
dictionary entry is the address and length of a string in the output
already written, so the decoder is a forward copy loop and needs no
reversal buffer. The decoder (about 190 bytes) lives in the cassette buffer,
the dictionary in the text screen ($0400-$07FF, including a table of the
256 literal values), so the program must not load into $01F0-$07FF. The
oldest entry is overwritten when the dictionary is full. The compressor
simulates the decoder and searches the code sequence with a beam search
(not just the longest match), and tries several intervals for clearing
the dictionary. Set `XIPZ_LZW_BEAM` (default 16) and `XIPZ_LZW_CAND`
(default 4) to widen the search; in tests this gained only a few
bytes. The format is described in `src/lzw.hh`; see `src/lzw.cc`,
`src/decrunchlzwstub.s` and `library/t7d/compression/xipz-lzw.decrunch.inc`.

## lzrc ##

LZ77 with the adaptive binary range coder of squz; the "maximum
compression" mode for general data. A token is a literal (an adaptive
bit tree, one tree after a literal and one after a match), a match with a
new offset or a "repeat" match which reuses the last offset. Offsets and
lengths are Elias-gamma numbers whose bits all have adaptive
probabilities. The compressor does an optimal parse (dynamic
programming over all positions with the cost of every token taken from the
statistics of the previous parse, iterated, the best real size wins);
`XIPZ_LZRC_CHAIN` (default 1024) and `XIPZ_LZRC_PASSES` (default 8)
change the search effort. The format is described in `src/lzrc.hh`.

The decoder is smaller than squz's (stub 446 bytes): the range decoder and
the Elias-gamma routine at the bottom of the stack page, the rest in the
cassette buffer, the probabilities in $0400-$067F, so the program must not
load into $0100-$07FF. Decoding is slow (the range decoder), but the
copying of matches is fast. Measured on four programs the data is 2-58 %
smaller than with squz and 2-40 % smaller than with bpe; on data
without long repeats (random data) and on 6502 code with few repeats (8 KB of
the C64 KERNAL ROM, 2 % larger) squz stays slightly better because of
its instruction position context. See
`src/lzrc.cc`, `src/decrunchlzrcstub.s` and for the raw format
`library/t7d/compression/xipz-lzrc.decrunch.inc`.

## lzh16 ##

LZ77 without an entropy coder: a plain bit stream (msb first) with
literals (a zero and eight bits), matches with a new offset and matches
with one of the last 16 new offsets (a four bit slot of a ring, the oldest
entry is overwritten). Lengths (2-256) and the high byte of an offset
(distances up to 65280) are interleaved Elias-gamma numbers read into an
eight bit register, the low byte of an offset is stored raw. The format is
described in `src/lzh16.hh`.

The stub is small and the decoder fast (about 270 cycles per output byte on
KERNAL-ROM-sized data): 243 bytes, the decoder in the cassette buffer
($0334-$03FF), the history in the text screen ($0400-$041F), so the
program must not load into $0334-$041F or $01F0-$01FF. The compressor is a
dynamic program that keeps the cheapest 16 histories at every position
(`XIPZ_LZH16_BEAM`, default 16; `XIPZ_LZH16_CHAIN`, default 256, is the
length of the hash chains); a beam of 64 gains only about 0.1 %. Measured
on three programs (another, carmdigi, searching) the data is 9133, 7445 and
701 bytes, against 9765, 7798, 1825 for lzw, 8948, 7256, 1504 for squz and
8605, 6912, 645 for lzrc: slightly worse than the range coders but
much faster and with a smaller stub. See `src/lzh16.cc`,
`src/decrunchlzh16stub.s` and for the raw format
`library/t7d/compression/xipz-lzh16.decrunch.inc` (5 bytes of zero page, 32
bytes of history).

## lzp ##

A variant of the LZ77 algorithm where the prediction buffer is a hash
buffer which doubles as storage for the hash and the matches, [see
e.g. Wikibooks](https://en.wikibooks.org/wiki/Data_Compression/Dictionary_compression#LZ77_algorithms).

A mask byte is written to specify for the next eight bytes if they are
literals or back references.

See main_lzp() function.

## lzp3 ##

A predictor in the spirit of lzp2 (one flag bit per byte, no offsets and
no lengths) with two models instead of one: a hashed order-2 model
(`(byte2 << 4) ^ byte1`) and an order-1 model indexed by the last byte.
Both are updated with every byte. The prediction of the order-2 model is
tried first, a zero entry counts as unknown and the order-1 entry is used
instead. If the first prediction was wrong and the order-1 entry is
another byte then it gets a second chance, which costs one more flag
bit. The flag bits are read MSB first from a mask byte that is fetched
when its first bit is needed, literals are interleaved. There is no end
marker, the stub stops at the end address like lzp2.

The stub is 212 bytes and lives in the text screen: the code at $0400, the
models at $0500 and $0600. Zero page $A4-$AA is used. Compared with lzp2
(stub included) the output is smaller on six of seven test programs, up to
12 % on the ones with many near-repeats (searching: 3568 against 4002
bytes), but 2 % larger on carmdigi where the stub is not paid back. See
`Lzp3Compressor` in `src/lzp.cc` (the compressor verifies its output with a
host decoder), `src/decrunchlzp3stub.s` and for the raw format
`library/t7d/compression/xipz-lzp3.decrunch.inc` (512 bytes of buffer).

## lzp4 ##

The original LZP: instead of predicting a single byte like lzp2 and lzp3
the table, indexed by a hash of the last bytes (`(hash << 4) ^ byte`, 8
bits), holds the 16 bit position in the output where this context
occurred last. If the data starting there is the same as the data to
come, a run is copied: a set flag bit and a length byte (length minus 2,
at most 255 bytes) are written, otherwise a cleared flag bit and the
literal. The table is updated for every output byte, also inside runs.
The run may overlap its own output. The flag bits are fetched like in
lzp3. There is no end marker.

The stub is 219 bytes and lives in the text screen: code at $0400, the low bytes
of the table at $0600 and the high bytes at $0700, every entry starts
with the load address. Zero page $A4-$A9. The decoder is not fast but
runs have a 9 bit overhead only, so it is the best of the lzp family on
data with long repeats (searching.2200: 1808 bytes against 3568 for lzp3
and 4002 for lzp2, infotext: 3218 against 3744) but worse on data
without them (another.standalone: 19177 against 17836 for lzp3). The
parameters are `LZP4_SHIFT` and `LZP4_MINLEN` in `src/lzp.cc` (and
`HASHSHIFT` and `MINLEN` in the stub), shift 4 and length 2 was best on
most files. See `Lzp4Compressor` in `src/lzp.cc`, `src/decrunchlzp4stub.s` and
`library/t7d/compression/xipz-lzp4.decrunch.inc` (512 bytes of table).

## lzp5 ##

lzp2 without the hash: a real order-1 context. The 256 byte table is
indexed by the last byte and holds the byte which followed it last. One
flag bit per byte says that the prediction was right, otherwise a
cleared flag bit and the literal are written. The rolling hash of lzp2
(`(hash << 5) ^ byte`, 8 bits) forgets everything older than the last two
bytes and keeps only three bits of the second-last byte, which for a 256
byte table is mostly noise. Measured with one 256 byte table the plain
order-1 context compressed better than lzp2's hash on six of seven
programs, and every order-2 and order-3 hash tried (other shifts, sums)
was about the same as lzp2 or worse.

The stub is 159 bytes (code at $0400, table at $0500, zero page
$A5-$AA), the smallest of the lzp family. Compared with lzp2 the output
(stub included) is smaller on all seven test programs, for example
2999 against 3126 bytes (dload) and 13108 against 13306 (another.2200);
compared with lzp3 it is smaller on four of the seven (the programs without
many near-repeats), and lzp4 still wins where there are long repeats. See
`Lzp5Compressor` in `src/lzp.cc`, `src/decrunchlzp5stub.s` and
`library/t7d/compression/xipz-lzp5.decrunch.inc` (256 bytes of buffer).

## tc ##

The format of the Time Cruncher V5 (Matcham/Network, V5 by Tim Rogers,
1991). The format was taken from the disassembly of its decruncher on
[codebase64](https://codebase64.net/doku.php?id=base:2mhz_time_crunch_v5_disassembled),
the compressor is new: the original searches the matches by brute force
in the memory, here the matches come from hash chains (for every length
the nearest distance, which is all that the costs depend on) and an
optimal parse (a dynamic program that knows the cost of every token)
chooses them. It takes less than a quarter of a second for 20 KB.

It is an LZ77 without an entropy coder that is **decrunched backwards**:
the decoder reads the packed data downwards and writes the output
downwards, starting with the last byte, and the matches copy from the
data that has been written before, that is from higher addresses. So
the packed data is placed at the bottom of the area of the output and
the output overtakes it from above, without a copy and without free
memory; the compressor computes how far below the load address the
packed data has to be (usually just one byte). Bits (MSB first) and raw
literal bytes share one stream. A token of three bits is a run of 1 to 5
literals, 6 to 21 (four more bits), 6 to 255 (eight bits) or an escape
for runs that are not followed by a match; a match has the length 2 (eight
bit offset), 3, or 4 to 255 (four or eight bits) and an offset of eight bits
or of 8+STEP bits. The distance is the offset plus the length, so a match
can not overlap itself (runs of one byte are coded with doubling
matches, which is the weak point of the format). STEP (1 to 8) is
chosen by the compressor for every file and stored in the decoder. See
`src/timecrunch.hh` for the exact format, `src/timecrunch.cc` for the compressor
(`XIPZ_TC_CHAIN` limits the length of the hash chains, default 4096;
`XIPZ_TC_DEBUG` with `-v` lists the parse).

The stub is 372 bytes, the largest one: the decoder is the one of the
original, 339 bytes in the text screen ($0400-$0552), plus the code that
moves the packed data to its place (in the direction that the addresses
need). The packed data is moved to just below the load address, so
the load address and this place must be $0600 or higher. Compared with
the other LZ77 variants (stub included, in bytes):

| file | lzh16 | tc | ratz | qadz | lzp4 |
|---|---|---|---|---|---|
| another.standalone | 12806 | 13480 | 14058 | 14863 | 19177 |
| searching.standalone | 4349 | 4892 | 5127 | 5827 | 6631 |
| carmdigi.2200 | 7688 | 7747 | 7913 | 8016 | 10314 |
| infotext.2200 | 2523 | 2841 | 2815 | 3116 | 3218 |
| another.2200 | 9376 | 9900 | 10168 | 10777 | 14355 |
| searching.2200 | 944 | 1304 | 1252 | 1741 | 1808 |
| dload | 2518 | 2736 | 2735 | 2815 | 3267 |

With the stubs tc is smaller than qadz on all seven programs and smaller
than ratz on four (ratz has the smaller stub: on infotext, dload and
searching.2200 it is smaller by 26, 1 and 52 bytes) but lzh16 is smaller
than tc on all of them. Without the stubs (`-r`) tc is smaller than ratz
and qadz on all seven programs, and lzh16 is smaller on six (carmdigi is the
exception, 7374 against 7445 bytes).

The raw data (`-r`) is the STEP followed by the packed data, to be
decrunched with `library/t7d/compression/xipz-tc.decrunch.inc`: it takes
the address of the last packed byte, of the last output byte and of the
STEP (the byte below the packed data); the data may overlap with the
output as described there. The decoder stops when it needs the byte
below the packed data, so there is no end marker; the unused bits at the end
are zero. The reference decoder of the compressor works on a memory
image with the packed data in place, so the overlap is checked. The
format and the compressor were also tested with the decruncher of the
original Time Cruncher (taken from the listing) running in an emulator.

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

## lzp ##

As this algorithm is mediocre at detecting backreferences (it was
designed to be used in modem hardware) precompressing with RLE will
make the result worse. But your mileage may vary.

# Links #

 * https://csdb.dk/release/?id=6646
 * https://github.com/pararaum/c64playground/tree/xipz/compression/XipZ
