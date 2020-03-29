; -*- mode: asm -*-

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
