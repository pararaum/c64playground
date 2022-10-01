
# Brainfuck for the Commodore 64 #

---

# What, Why, and WTF? #

  * Introduced by Urban Müller in 1993
  * Slower than BASIC
  * Extremely difficult 🤯
  * Provides ample opportunity to shoot yourself in the foot
  * Other interpreters for the C64 are available, this one provides an "IDE"

---

# Eight elementary commands #

## I/O ##

 * `.` prints out the character with the ASCII value at the current memory cell.
 * `,` reads a character from keyboard and puts ASCII code into the current memory cell.

---

# Eight elementary commands #

## Cell Operations ##

 * `+` increases the value at the current memory cell by 1. If the maximum cell value (typically 255) is exceeded the value wraps to 0.
 * `-` decreases the value at the current memory cell by 1. If the cell value already contains its minimum value (0), the value wraps to the maximum cell value (typically 255).

 * `>` increases the pointer to the current memory cell by 1
 * `<` decreases the pointer to the current memory cell by 1

---

# Eight elementary commands #

## Flow Control ##

* `[` if the current memory cell contains 0, the program flow jumps forward after the corresponding `]` command. If the current memory cell contains any other number than 0, the program continues with the command right after the `[`.
 * `]` if the current memory cell contains any other number than 0, the program flow jumps backward after the corresponding `[`. If the current memory cell contains 0, the program continues with the command right after the `]`.

---

# Usage #

A disk image is provided, it is called: `brainfuck64.d64`. Use `LOAD "BRAINFUCK64.PRG",8` and start the interpreter with "`RUN`". After the screen turns green the interpreter awaits your commands.

Then use line numbers to program you Brainfuck program as you would in BASIC. Commands like `LOAD`, `SAVE`, `RUN`, etc. are available. Always remember that the primary purpose of Brainfuck is to make your head explode 🤯.

For a full description see the fully fledged documentation provided as a PDF file.

---

# First 🤯 #

Type in the following program:

`10 -[------->+<]>-.[-]   `

`20 --[----->+<]>-.[-]	  `

`30 +[------->++<]>--.[-] `

`40 +[------->++<]>--.[-] `

`50 +[------->++<]>+.     `

Type `LIST` to check if everything is correct. To run the program type `RUN`.

---

# The End #

  * Questions, Comments, Remarks?
  * Thank you for your attention!
