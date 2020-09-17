;;; Animate characters (2X2) and move them horizontally.

	.include	"kernal.i"
	.include	"irqmoiety.i"
	.include	"vicmacros.i"
	.include	"animate_char.i"
	.import	muzak_init
	.macpack	generic

ANIMATE_CHAR_SCREEN = $0400
ANIMATE_CHAR_CHARGEN = $0800

	.export	animate_char_chargenaddr

	animate_char_chargenaddr := ANIMATE_CHAR_CHARGEN

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

	.segment	"FONT"
	.incbin	"pacanifont.64c"

	.data
next_ac_y_val:
	.byte	0

	.code
setup_vic:
	lda	#0
	sta	$d020
	sta	$d021
	SetChargenAddress	ANIMATE_CHAR_CHARGEN
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

	.data
lfsr_value:	.byte	$ff
	.code
lfsr:	lda	lfsr_value
	lsr
	bcc	@skip
	eor	#$96
	@skip:
	sta	lfsr_value
@l:	cmp	#25
	bcs	@reduce
	rts
@reduce:	sub	#24
	bne	@l
	.ifndef NDEBUG
	.byte	$12
	.endif
	
	.code
_main:
	;; Fooling around with the stack...
	;; 	ldx	#$40
	;; 	txs
	jsr	setup_vic
	jsr	setup_irq
	lda	#0
	jsr	muzak_init
	jsr	animate_char_initialise
	cli
	lda	#0
	sta	$dd08		; Set the 1/10s of the TOD and turn clock on.
@wait:
	lda	$dd08		; Get current 1/10s value
	cmp	#7		; Trigger value reached?
	bne	@skip
	lda	#0
	sta	$dd08		; Set to zero again.
	jsr	lfsr
	;; 	sei		; Not needed, code is safe.
	jsr	animate_char_create
	;; 	cli
@skip:
	jsr	GETIN
	beq	@wait
	jmp	RESTOR		; Restore the original kernal vectors.

