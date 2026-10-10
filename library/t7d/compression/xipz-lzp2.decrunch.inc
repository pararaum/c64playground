; -*- mode: asm -*-

;;; Decrunch routine for the lzp2 cruncher (xipz -r -a lzp2).
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1, ptr2, ptr3) are used. The routine needs a buffer of
;;;	256 bytes, define XIPZMODEL as its address before including this
;;;	file. Otherwise the buffer is reserved in the BSS segment.
;;;
;;; The stream has no end marker, so the end of the output must be given.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;;	- XIPZENDPTR: must point to the address behind the last byte to produce
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
	.ifndef	XIPZENDPTR
XIPZENDPTR = ptr3
	.endif
	.ifndef	XIPZMODEL
	.pushseg
	.bss
XIPZMODEL:	.res	256
	.popseg
	.endif

.proc	decrunch_lzp2
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
predict:			; Predict the next value.
	ldx	hash
	lda	XIPZMODEL,x
	jsr	output
	jsr	advance
	jmp	decrunchloop
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
update:				; Modifies: A, X
	ldx	hash
	sta	XIPZMODEL,x
advance:			; Modifies: A
	asl	hash
	asl	hash
	asl	hash
	asl	hash
	asl	hash
	eor	hash
	sta	hash
	rts
output:				; Modifies: X
	sta	a:$0000
	dest = *-2
	inc	dest
	bne	op_chk
	inc	dest+1
op_chk:	ldx	dest		; Reached the end?
	cpx	z:XIPZENDPTR
	bne	op_out
	ldx	dest+1
	cpx	z:XIPZENDPTR+1
	beq	finished
op_out:	rts
finished:
	pla			; Drop the return address of output.
	pla
	lda	dest
	sta	z:XIPZDSTPTR
	lda	dest+1
	sta	z:XIPZDSTPTR+1
	rts
hash:	.byte	0		; Current hash value.
maskidx:	.byte	0	; Next mask bit.
mask:	.byte	0		; Current mask.
.endproc
