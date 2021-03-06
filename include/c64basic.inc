; -*- mode: asm -*-
;;; See https://sta.c64.org/cbm64basconv.html
;;; See http://unusedino.de/ec64/technical/project64/mapping_c64.html

	;; Basic variables
	;; ═══════════════

	;; Flag: Type of Data (String or Numeric)
	VALTYP	= $0D
	
	;; Basic vectors
	;; ═════════════

	IERROR 	= $0300
	;; http://unusedino.de/ec64/technical/project64/mapping_c64.html calls this IGONE
	IGONE	= $0308
	IEVAL	= $030a
	;; Vector for the USR() function.
	USRADD	= $0311

	;; Basic routines
	;; ══════════════
	CHRGET	= $0073
	CHRGOT	= $0079
	;;
	NEWSTT 	= $a7ae		; Set Up Next Statement for Execution.
	GONE 	= $A7E4		; Read and Execute the Next Statement.
	STROUT 	= $AB1E		; Output a string pointed to by A/Y, see L. Englisch, The Advanced Machine Language Book for the Commodore-C64, Abacus Software, 1984, p. 21.
	;; See also https://www.pagetable.com/c64ref/c64disasm/.
	;; FREFAC
	FREFAC	= $B6A6		; Free the string in FACMO?
