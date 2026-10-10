; -*- mode: asm -*-

;;; Decrunch routine for the lzp5 cruncher (xipz -r -a lzp5).
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

.proc	decrunch_lzp5
	lda	#0
	tay
@clear:	sta	XIPZMODEL,y	; Clear the model.
	dey
	bne	@clear
	sta	last
	sta	mask		; Forces the first mask byte to be read.
	lda	z:XIPZDSTPTR
	sta	dest
	lda	z:XIPZDSTPTR+1
	sta	dest+1
decrunchloop:
	ldx	last		; The context is the last byte.
	lda	XIPZMODEL,x
	sta	pred
	jsr	getbit
	bcc	literal
	lda	pred		; Hit, carry is still set.
	bcs	emit		; Always.
literal:
	jsr	next_byte
emit:	jsr	output
	ldx	last		; Update the model.
	sta	XIPZMODEL,x
	sta	last
	jmp	decrunchloop
getbit:				; Next mask bit into the carry.
	asl	mask
	bne	gb_out
	jsr	next_byte	; Only the sentinel was left, get new mask.
	sec
	rol
	sta	mask
gb_out:	rts
next_byte:			; Get next byte.
	lda	(XIPZSRCPTR),y
	iny
	bne	nb_out
	inc	z:XIPZSRCPTR+1
nb_out:	rts
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
last:	.byte	0		; The last byte, the context.
mask:	.byte	0		; Current mask bits with sentinel.
pred:	.byte	0		; Current prediction.
.endproc
