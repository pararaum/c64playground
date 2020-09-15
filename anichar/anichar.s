;;; Animate characters (2X2) and move them horizontally.

	.include	"kernal.i"
	.include	"irqmoiety.i"
	.include	"vicmacros.i"
	.import	muzak_init

ANIMATE_CHAR_SCREEN = $0400

;;; Macro to clear 1000 Bytes, e.g. a textscreen.
;;; Modifies: A/X
	.macro	Clear_1000_bytes	begin,fillval
	.local	mloop
	lda	#fillval
	ldx	#0
mloop:	sta	begin,x
	sta	begin+$100,x
	sta	begin+$200,x
	sta	begin+$300-24,x
	dex
	bne	mloop
	.endmacro


	.export	_main

_main:
	;; Fooling around with the stack...
	;; 	ldx	#$40
	;; 	txs
	jsr	setup_vic
	jsr	setup_irq
	lda	#0
	jsr	muzak_init
	cli
@wait:	jsr	GETIN
	beq	@wait
	jmp	RESTOR

setup_vic:
	lda	#0
	sta	$d020
	sta	$d021
	SetChargenAddress	$800
	lda	#$9b		; Yellow colour + multicolour mode (11).
	jsr	CHROUT
	lda	#$93		; Clear
	jsr	CHROUT
	lda	#6		; Blue
	sta	$d023		; 10
	lda	#13		; Light Green (unused)
	sta	$d022		; 01
	Clear_1000_bytes	ANIMATE_CHAR_SCREEN,$ff
	lda	#%00011000	; Multicolour mode, 40 Columns, X-Scroll=0
	sta	$d016
	rts

