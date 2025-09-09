# Prepender64 #

This is simple tool to prepend existing programs with a stub which
does an old-school effect while copying the original program at the
desired destination. One of the copying stubs is visible at the top of
the text screen and will show the "rotating characters". Rather
useless but old school.

Further on the tool can be used as a simple linker.

# Usage #

```
Usage: Prepender64 [OPTION]... [FILE]...

  -h, --help            Print help and exit
  -V, --version         Print version and exit
  -o, --output=STRING   output file name, defaults to first input filename with
                          .prep suffix
  -j, --jump=INT        address to jump to (-1 = SYS address)  (default=`-1')
  -J, --loadjump        jump to the load address  (default=off)
  -e, --eor=INT         eor value used to obfuscate, if applicable
                          (default=`0')

available modes:

 Mode: copy eor
  copy the file and eor it with a value, the code is at the top of the text
  screen
      --copy-eor        copy and eor mode

 Mode: copy eor stack
  copy the file and eor it with a value, the code is in the stack
      --copy-eor-stack  copy and eor mode in stack

 Mode: do not spread
  outputs the message 'do not spread' and then run the original
      --donotspread     do not spread message-mode

 Mode: scrambler16
  scramble the code with a 16-bit LFSR
      --scrambler       scramble the code with a 16-bit LFSR
      --scrambler-feedback=LONG feedback term for the LFSR  (default=`0x8117')
      --scrambler-start=LONG    start term for the LFSR  (default=`0xF77D')

 Mode: autostart $326
  autostart with code in the cassette buffer
      --autostart326    autostart at $326

 Mode: vcclogo
  prepend a VCC logo
      --vcclogo         prepend VCC logo
```

The filename has a special format it can be one of:

   * filename,addr,offset,length
   * filename,,offset,length
   * filename,addr,offset
   * filename,,offset
   * filename,addr
   * filename@addr,offset,length
   * filename@addr,offset
   * filename@addr

If given then addr will override the load address of the file, if
address is given with "@" then the file is considered a raw file
without a loadadress and the given load address is used. Offset is
used to skip bytes starting from the beginning and length can be used
to only load the first length bytes.

Prepend a debug version of a program with a stub which shows the text
"Do not spread!": `prepender64 --donotspread input.prg`.

Use the Prepender64 to link several files together and use the eor obfuscation: `prepender64 --copy-eor -e 0x70 -o out.prg inp1.prg inp2.prg inp3.prg`.

## Available Stubs ##

### Copy Eor ###

Copy the program to the destination address with the stub at the top
of the screen and the rolling character effect.

### Copy Eor Stack ###

Copy the program to the destination address with the stub in the
stack.

### Do Not Spread ###

Print a message "Do not spread!" and perform the copying operation.

### Scrambler16 ###

This mode uses a 16bit-LFSR to encode the program. Use it last as the
resulting file will probably be uncompressible...

The feedback and the start term can be set for the scrambler. The
start term must be not equal to zero. Warning, chose the terms
carefully to get the maximum length LFSR run. For a list LFSR terms
see <https://users.ece.cmu.edu/~koopman/lfsr/index.html>.

### Autostart $326 ###

Autostart a programm via the CHROUT vector.

### VCC Logo ###

Prepend a VCC logo as a LIST-Art.

# Docker Image #

In the VCC C64 build-tools Docker image the current version of the
`Prepender64` is available for convenience usage. The Docker image is
available as "vintagecomputingcarinthia/c64build", see
https://hub.docker.com/r/vintagecomputingcarinthia/c64build. Just use
`docker pull docker pull vintagecomputingcarinthia/c64build:latest`
for the latest image.

In order to get the help use the following command line:

```
docker run --rm -it vintagecomputingcarinthia/c64build prepender64 -h
```

As an example we will prepend a simple copy-eor-stub to our ["Hand Watch You"](https://csdb.dk/release/?id=226332 "https://csdb.dk/release/?id=226332") demo:

```
docker run -u 1000:uucp --rm -it -v $PWD:/host -w /host vintagecomputingcarinthia/c64build prepender64 --copy-eor hands_watch_you.prg
```

This will give the following output:

	Prepender64 Version 0.0.0
	24871 bytes have been read.
	Load address is $0801.
	Data is from $0801 to $6925 (24869 $6125 bytes).
	Determined a value of $080D (2061) for JMP from SYS line.
	🎵I am the great prepender🎶...

# Building #

A C++ compiler, `gengetopt` are needed, build with `make`.
