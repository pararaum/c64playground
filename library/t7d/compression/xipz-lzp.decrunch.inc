; -*- mode: asm -*-

;;; Decrunch routine for the lzp cruncher (xipz -r -a lzp).
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1, ptr2) are used. The routine needs a buffer of 256
;;;	bytes, define XIPZMODEL as its address before including this file.
;;;	Otherwise the buffer is reserved in the BSS segment.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, the 256 byte buffer
;;; Output: XIPZDSTPTR points behind the last decrunched byte.
;;;
;;; The state is kept inside the routine, therefore it must be located in RAM.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	XIPZMODEL
	.pushseg
	.bss
XIPZMODEL:	.res	256
	.popseg
	.endif

.proc	decrunch_lzp
	lda	#0
	tay
@clear:	sta	XIPZMODEL,y	; Clear the model.
	dey
	bne	@clear
	sta	hash
	sta	maskidx		; Forces the first mask to be read.
	lda	z:XIPZDSTPTR
	sta	dest
	lda	z:XIPZDSTPTR+1
	sta	dest+1
decrunchloop:
next_mask:			; Get the next mask bit.
	asl	maskidx
	bne	nm_stillgoing
	lda	#1
	sta	maskidx
	jsr	next_byte
	sta	mask
nm_stillgoing:
	lda	maskidx
	and	mask
	beq	literal
	jsr	next_byte	; Get run length.
	cmp	#0
	beq	finished	; Length zero marks the end.
	sta	length
replayloop:
	ldx	hash		; Predict the next value.
	lda	XIPZMODEL,x
	jsr	output
	jsr	advance
	dec	length
	bne	replayloop
	beq	decrunchloop	; Always.
literal:
	jsr	next_byte
	jsr	output
	jsr	update
	jmp	decrunchloop
next_byte:			; Get next byte.
	lda	(XIPZSRCPTR),y
	iny
	bne	nb_out
	inc	z:XIPZSRCPTR+1
nb_out:	rts
finished:
	lda	dest
	sta	z:XIPZDSTPTR
	lda	dest+1
	sta	z:XIPZDSTPTR+1
	rts
update:				; Modifies: A, X
	ldx	hash
	sta	XIPZMODEL,x
advance:			; Modifies: A
	asl	hash
	asl	hash
	asl	hash
	clc
	adc	hash
	sta	hash
	rts
output:				; Modifies: -
	sta	a:$0000
	dest = *-2
	inc	dest
	bne	op_out
	inc	dest+1
op_out:	rts
hash:	.byte	0		; Current hash value.
maskidx:	.byte	0	; Next mask bit.
mask:	.byte	0		; Current mask.
length:	.byte	0		; Current replay length.
.endproc
