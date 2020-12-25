; -*- mode: asm -*-
;;; See https://sta.c64.org/cbm64basconv.html
;;; See http://unusedino.de/ec64/technical/project64/mapping_c64.html
;;; A list can be found in Brückmann et al, 64 Intern, Data Becker, 1988, p 45. Or [https://www.c64-wiki.com/wiki/BASIC-ROM].

	;; Basic vectors
	;; ═════════════

	IERROR = $0300
	;; http://unusedino.de/ec64/technical/project64/mapping_c64.html calls this IGONE
	IGONE	= $0308
	IEVAL	= $030a

	;; Basic routines
	;; ══════════════
	CHRGET	= $0073
	CHRGOT	= $0079
	;;
	NEWSTT = $a7ae		; Set Up Next Statement for Execution.
	GONE = $A7E4		; Read and Execute the Next Statement.
	STROUT = $AB1E		; Output a string pointed to by A/Y, see L. Englisch, The Advanced Machine Language Book for the Commodore-C64, Abacus Software, 1984, p. 21.
	;; See also https://www.pagetable.com/c64ref/c64disasm/.

	;; D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, Compute!'s Publication, 1984, p 89.
	INIT_BASIC_VECTORS = $E453 ; Init basic vectors $0300-$030b (RVECT?)
	;; Move memory. [D. Heeb, Compute!'s VIC-20 and Commodore 64 Tool Kit: BASIC, Compute!'s Publication, 1984, p 123.
	;; $5f = pointer to src start
	;; $5A = pointer to src end+1
	;; $58 = pointer to destination end+1
	;; OUT: $58 = pointer to destination start-$100(!!!), the book seems to be wrong here!
	;;	X=0, Y=0
	MEMORY_MOVE = $A3BF
