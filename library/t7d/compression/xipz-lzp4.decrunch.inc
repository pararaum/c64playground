; -*- mode: asm -*-

;;; Decrunch routine for the lzp4 cruncher (xipz -r -a lzp4).
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1, ptr2, ptr3) are used. The routine needs a table of
;;;	512 bytes (256 low bytes of positions, then 256 high bytes), define
;;;	XIPZMODEL as its address before including this file. Otherwise the
;;;	table is reserved in the BSS segment.
;;;
;;; The stream has no end marker, so the end of the output must be given.
;;; The hash shift and the minimum run length must be the same as in lzp.cc.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;;	- XIPZENDPTR: must point to the address behind the last byte to produce
;;; Modifies: A, X, Y, XIPZSRCPTR, the 512 byte table
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

XIPZTABLO = XIPZMODEL
XIPZTABHI = XIPZMODEL+256
XIPZMINLEN = 2

.proc	decrunch_lzp4
	lda	z:XIPZDSTPTR
	sta	dest
	lda	z:XIPZDSTPTR+1
	sta	dest+1
	ldy	#0
	sty	hash
	sty	mask		; Forces the first mask byte to be read.
@init:	lda	dest		; Every entry points to the start of the output.
	sta	XIPZTABLO,y
	lda	dest+1
	sta	XIPZTABHI,y
	dey
	bne	@init
decrunchloop:
	jsr	getbit
	bcc	literal
	jsr	next_byte	; Run: length minus XIPZMINLEN.
	clc
	adc	#XIPZMINLEN
	sta	len
	ldx	hash		; Where did this context occur last?
	lda	XIPZTABLO,x
	sta	copysrc
	lda	XIPZTABHI,x
	sta	copysrc+1
copyloop:
	lda	a:$0000
	copysrc = *-2
	jsr	put
	inc	copysrc
	bne	cp_nocarry
	inc	copysrc+1
cp_nocarry:
	dec	len
	bne	copyloop
	beq	decrunchloop	; Always.
literal:
	jsr	next_byte
	jsr	put
	jmp	decrunchloop
put:				; Output A, update table and hash.
	sta	byte
	ldx	hash
	lda	dest		; Position of this byte.
	sta	XIPZTABLO,x
	lda	dest+1
	sta	XIPZTABHI,x
	lda	hash
	asl			; Shift by 4, see lzp.cc.
	asl
	asl
	asl
	eor	byte
	sta	hash
	lda	byte
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
finished:
	pla			; Drop the return address of put.
	pla
	lda	dest
	sta	z:XIPZDSTPTR
	lda	dest+1
	sta	z:XIPZDSTPTR+1
	rts
hash:	.byte	0		; Hash of the last bytes.
mask:	.byte	0		; Current mask bits with sentinel.
len:	.byte	0		; Remaining length of the run.
byte:	.byte	0		; Byte being output.
.endproc
