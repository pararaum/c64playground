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
debug_copy:
	ldx	#0
	ldy	#6
	jsr	animate_char_putat
	ldx	#1
	ldy	#8
	jsr	animate_char_putat
	ldx	#2
	ldy	#11
	jsr	animate_char_putat
	ldx	#3
	ldy	#14
	jsr	animate_char_putat
	ldx	#4
	ldy	#17
	jsr	animate_char_putat
	ldx	#5
	ldy	#20
	jsr	animate_char_putat
	ldx	#3
@l1:	txa
	sta	$0400,x
	sta	$0400+23*40,x
	dex
	bpl	@l1
	lda	#4
	sta	$0400+40
	sta	$0400+40+23*40+0
	add	#1
	sta	$0400+40+1
	sta	$0400+40+23*40+1
	add	#1
	sta	$0400+40+2
	sta	$0400+40+23*40+2
	add	#1
	sta	$0400+40+3
	sta	$0400+40+23*40+3
	rts
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
	jsr	debug_copy
	lda	#0
	sta	$dd08		; Set the 1/10s of the TOD and turn clock on.
@wait:
	lda	$dd08		; Get current 1/10s value
	cmp	#4		; Trigger value reached?
	bne	@skip
	lda	#0
	sta	$dd08		; Set to zero again.
	lda	next_ac_y_val
	adc	#7
@modloop:
	cmp	#25
	bcc	@s2
	sub	#25
	jmp	@modloop
@s2:	sta	next_ac_y_val
	jsr	animate_char_create
@skip:
	jsr	GETIN
	beq	@wait
	jmp	RESTOR		; Restore the original kernal vectors.

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

