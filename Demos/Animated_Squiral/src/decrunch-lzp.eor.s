	.include	"t7d/basic.i"
	.include	"LAMAlib-macros16.inc"
	.include	"globals.i"

	.export	decrunchLZP_to_E000

	.zeropage
;;; Current hash value.
HASH:	.res	1
;;; Source data pointer. (2 Bytes)R
DSTPTR:	.res	2
;;; Current replay length.
LENGTH:	.res	1
;;; Next Mask bit number.
MASKIDX:	.res	1
;;; Current Mask.
MASK:	.res	1

	.bss
model:	.res	256
	


	.code
;;; Decrunch routine for the qadz cruncher.
;;; Input: SRCPTR
decrunchLZP_to_E000:
	stax	SRCPTR
	ldax	#GFX_bitmapaddr
	stax	DSTPTR

	;; Init the Mask Index and Hash.
	ldy	#0
	sty	MASKIDX
	sty	HASH

	tya			; And now clear the model!
clmolo:	sta	model,y
	iny
	bne	clmolo
	;; Enter with Y=0. See next byte.
decrunchloop:
next_mask:			; Get the next mask bit.
	asl	MASKIDX
	bne	nm_stillgoing
	lda	#1
	sta	MASKIDX
	jsr	next_byte
	sta	MASK
nm_stillgoing:
	lda	MASKIDX
	and	MASK
	beq	literal
	jsr	next_byte	; Get run length.
	cmp	#0
	bne	replay
	rts			; We are done
replay:
	;; A=length.
	sta	LENGTH
replayloop:
predict:			; Predict the next value.
	ldx	HASH
	lda	model,x
	jsr	output
	jsr	advance
	dec	LENGTH
	bne	replayloop
	beq	decrunchloop
literal:
	jsr	next_byte
	jsr	output
	jsr	update
	jmp	decrunchloop
	;; Get next byte.
next_byte:
	lda	64738		; Dummy value
SRCPTR=*-2
	inc	SRCPTR
	bne	nb_out
	inc	SRCPTR+1
nb_out:	rts
update:
	ldx	HASH
	sta	model,x
advance:
	asl	HASH
	asl	HASH
	asl	HASH
	clc
	adc	HASH
	sta	HASH
	rts
output:
	tax			; Keep current value safe.
	eor	(DSTPTR),y
	sta	(DSTPTR),y
	txa			; Restore old value.
	iny
	bne	op_out
	inc	DSTPTR+1
op_out:	rts
