; -*- mode: asm -*-
;;; See https://sta.c64.org/cbm64basconv.html
;;; See http://unusedino.de/ec64/technical/project64/mapping_c64.html
;;; A list can be found in Brückmann et al, 64 Intern, Data Becker, 1988, p 45. Or [https://www.c64-wiki.com/wiki/BASIC-ROM].

	;; Basic pointers
	;; ══════════════

	;; Pointer to the start of BASIC program text
	;; https://pagetable.com/c64ref/c64mem/#002B
	TXTTAB = $2b
	;; Pointer to the start of BASIC variable storage
	;; https://pagetable.com/c64ref/c64mem/#002D
	VARTAB = $2d

	;; Basic vectors
	;; ═════════════

	;; https://pagetable.com/c64ref/c64mem/#0300
	IERROR = $0300
	;; http://unusedino.de/ec64/technical/project64/mapping_c64.html calls this IGONE
	IGONE	= $0308
	IEVAL	= $030a

	;; Basic routines
	;; ══════════════
	CHRGET	= $0073
	CHRGOT	= $0079

	;; Relink BASIC code
	;; Brückmann, et al., 64 Intern, Data Becker, 1983, p. 46.
	;; https://pagetable.com/c64ref/c64disasm/#A533
	BASIC_RELINK = $A533
	;; BASIC CLR
	;; https://pagetable.com/c64ref/c64disasm/#A659
	BASIC_CLR = $A659
	;; Execute Basic RUN.
	;; https://pagetable.com/c64ref/c64disasm/#A7AE
	BASIC_RUN = $A7AE
	NEWSTT = $a7ae		; Set Up Next Statement for Execution.
	GONE = $A7E4		; Read and Execute the Next Statement.
	;;  Using -t c64 does define some constants.
	.ifndef STROUT
	STROUT = $AB1E		; Output a string pointed to by A/Y, see L. Englisch, The Advanced Machine Language Book for the Commodore-C64, Abacus Software, 1984, p. 21. Warning! String size must be less than 256 bytes!
	;; See also https://www.pagetable.com/c64ref/c64disasm/.
	.endif
	;; Convert floating number to a string stored at $100.
	;; see: https://codebase64.org/doku.php?id=base:asm_include_file_for_basic_routines
	;; see: Dan Heeb, Compute!'s VIC-20 and Commodore C64 Tool Kit: BASIC, Compute!Publications, 1984, p. 233.
	;; Input: FAC1
	;; Output: A=0, Y=$1, String at $100.
	;; Modifies: A/X/Y, FAC1(!)
	BASIC_FLOAT_OUT = $BDDD

	;; D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, Compute!'s Publication, 1984, p 89.
	INIT_BASIC_VECTORS = $E453 ; Init basic vectors $0300-$030b (RVECT?)
	;; Move memory. [D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, Compute!'s Publication, 1984, p 123.
	;; $5f = pointer to src start
	;; $5A = pointer to src end+1
	;; $58 = pointer to destination end+1
	;; OUT: $58 = pointer to destination start-$100(!!!), the book seems to be wrong here!
	;;	X=0, Y=0
	MEMORY_MOVE = $A3BF

	;; Basic floating-point routines and macros
	;; ════════════════════════════════════════

	;; Lothar Englisch, Das Maschinensprachbuche Buch für Fortgeschrittene zum C64&PC128, Data Becker, 1985. Table with functions on page 67.

	;; Load FAC1 with signed 8-bit value.
	BASIC_FAC1_WITH_INT8 = $BC3C
	BASIC_FAC1_WITH_UINT8 = $B3A2

	;; see also: Lothar Englisch, The Advanced Machine Language Book for the Commodore 64, Abacus Software, 1984, p. 22.
	.macro	Fac1LoadFromByte byte
	.if byte < 0
	 lda	#(256+byte)
	 jsr	BASIC_FAC1_WITH_INT8
	.else
	 ldy	#(byte)
	 jsr	BASIC_FAC1_WITH_UINT8
	.endif
	.endmacro

	;; Load ARG (FAC2) from memory pointed to by A=LO, Y=HI
	;; Input: A/Y = pointer to variable
	;; Output: A=FAC exponent(?), ARG
	;; Modifies: A, Y
	;; see: Lothar Englisch, Das Maschinensprachbuche Buch für Fortgeschrittene zum C64&PC128, Data Becker, 1985, p. 67.
	;; see: D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, 1984, p. 20.
	BASIC_MEMARG = $BA8C

	;; Load FAC1 from memory pointed to by A=LO, Y=HI
	;; Input: A/Y = pointer to variable
	;; Output: A=FAC exponent(?), FAC1
	;; Modifies: A, Y
	;; see: Lothar Englisch, Das Maschinensprachbuche Buch für Fortgeschrittene zum C64&PC128, Data Becker, 1985, p. 67.
	;; see: D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, 1984, p. 20.
	;; see: https://github.com/mrdudz/cc65-floatlib/blob/master/float-c64.inc
	;; see: https://pagetable.com/c64ref/c64disasm/#BBA2
	BASIC_MEMFAC = $BBA2
