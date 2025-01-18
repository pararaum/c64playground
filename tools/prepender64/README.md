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

 Mode: autostart $326
  autostart with code in the cassette buffer
      --autostart326    autostart at $326

 Mode: vcclogo
  prepend a VCC logo
      --vcclogo         prepend VCC logo
```

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

### Autostart $326 ###

Autostart a programm via the CHROUT vector.

### VCC Logo ###

Prepend a VCC logo as a LIST-Art.

# Building #

A C++ compiler is needed, build with `make`.
