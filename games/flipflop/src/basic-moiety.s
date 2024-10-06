	.include	"LAMAlib-macros16.inc"
	.include	"LAMAlib-routines.inc"
	.include	"t7d/basic.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/pseudo/loadstorereg.i"

	.import	__BASIC_RUN__

FLIPTABLE = $200

	.export	setup_basic_program
	.export	_mkfliptable

	.bss
	;; Basic messes up the return address therefore we store it here.
oldreturn:
	.res	2
oldstackptr:
	.res	1

	.code
.proc	_mkfliptable
	pla
	sta	oldreturn
	pla
	sta	oldreturn+1
	tsx
	stx	oldstackptr
	;; Clear the table (table in the entry buffer)
	jsr	clear_memory_progmem
	.word	FLIPTABLE
	.word	89
	jsr	BASIC_CLR
	jsr	BASIC_RUN
	.byte	2		; Never reached.
.endproc

assembler_reentry:
	ldx	oldstackptr
	txs
	lda	oldreturn+1
	pha
	lda	oldreturn
	pha
	ldax	#FLIPTABLE
	rts


	.code
.proc	setup_basic_program
	pla
	sta	oldreturn
	pla
	sta	oldreturn+1
	;; Set basic
	ldax	#__BASIC_RUN__+1
	stax	TXTTAB
	ldax	#BASIC_END
	stax	VARTAB
	ldax	#assembler_reentry
	stax	IERROR
	jsr	BASIC_RELINK
	jsr	BASIC_CLR
	lda	oldreturn+1
	pha
	lda	oldreturn
	pha
	rts
.endproc

	;; BASIC program used for calculating the "flips".
	.segment	"BASIC"
	.byte	0
	.incbin	"mkfliptable.bas",2
BASIC_END:
