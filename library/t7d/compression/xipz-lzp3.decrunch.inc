; -*- mode: asm -*-

;;; Decrunch routine for the lzp3 cruncher (xipz -r -a lzp3).
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1, ptr2, ptr3) are used. The routine needs a buffer of
;;;	512 bytes (order-2 model first, then the order-1 model), define
;;;	XIPZMODEL as its address before including this file. Otherwise the
;;;	buffer is reserved in the BSS segment.
;;;
;;; The stream has no end marker, so the end of the output must be given.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;;	- XIPZENDPTR: must point to the address behind the last byte to produce
;;; Modifies: A, X, Y, XIPZSRCPTR, the 512 byte buffer
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
XIPZMODEL:	.res	512
	.popseg
	.endif

XIPZMODEL2 = XIPZMODEL		; Order-2 model, hashed.
XIPZMODEL1 = XIPZMODEL+256	; Order-1 model.

.proc	decrunch_lzp3
	lda	#0
	tay
@clear:	sta	XIPZMODEL1,y	; Clear the models.
	sta	XIPZMODEL2,y
	dey
	bne	@clear
	sta	byte1
	sta	byte2
	sta	mask		; Forces the first mask byte to be read.
	lda	z:XIPZDSTPTR
	sta	dest
	lda	z:XIPZDSTPTR+1
	sta	dest+1
decrunchloop:
	lda	byte2		; hash = (byte2 << 4) ^ byte1
	asl
	asl
	asl
	asl
	eor	byte1
	sta	hash
	tax
	lda	XIPZMODEL2,x
	bne	have2
	ldx	byte1		; Nothing known for the order-2 context.
	lda	XIPZMODEL1,x
	sta	pred
	jsr	getbit
	bcs	hit
	bcc	literal
have2:	sta	pred
	jsr	getbit
	bcs	hit
	ldx	byte1		; Missed, second chance with the order-1 byte.
	lda	XIPZMODEL1,x
	cmp	pred
	beq	literal		; Same byte, was a miss already.
	sta	pred
	jsr	getbit
	bcc	literal
hit:	lda	pred
	bcs	emit		; Always.
literal:
	jsr	next_byte
emit:	jsr	output
	ldx	hash		; Update both models.
	sta	XIPZMODEL2,x
	ldx	byte1
	sta	XIPZMODEL1,x
	stx	byte2
	sta	byte1
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
hash:	.byte	0		; Hash of the last two bytes.
byte1:	.byte	0		; The last byte.
byte2:	.byte	0		; The byte before.
mask:	.byte	0		; Current mask bits with sentinel.
pred:	.byte	0		; Current prediction.
.endproc
