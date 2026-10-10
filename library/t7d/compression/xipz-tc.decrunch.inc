; -*- mode: asm -*-

;;; Decrunch routine for the tc cruncher (xipz -r -a tc), the format
;;; of the Time Cruncher V5 (see compression/XipZ/timecrunch.hh).
;;;
;;; The data is decrunched BACKWARDS: the routine reads the packed data
;;; downwards and writes the output downwards, starting with the last
;;; byte. The first byte of the raw data of xipz is the STEP, the
;;; packed data follows. The decoder stops when it has used up the data.
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1 to ptr4) are used.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to the LAST byte of the packed data
;;;	- XIPZDSTPTR: must point to the LAST byte of the output
;;;	- XIPZENDPTR: must point to the first byte of the raw data (the
;;;	  STEP), that is the byte below the packed data
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, XIPZCOPYPTR
;;; Output: XIPZDSTPTR points to the byte below the first byte of the output.
;;;
;;; The packed data may be inside the area of the output if the
;;; compressor (xipz -v) says so: the packed data is then placed at
;;; the bottom of the output area, with the last packed byte at least
;;; "gap" bytes below the last output byte. Without overlap it works
;;; anywhere, the output must not overwrite the packed data.
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
	.ifndef	XIPZCOPYPTR
XIPZCOPYPTR = ptr4
	.endif

.proc	decrunch_tc
	ldy	#0
	lda	(XIPZENDPTR),y	; The STEP.
	clc
	adc	#7
	sta	tab+2
	sty	repeat
	sty	bitcnt
main:	ldx	#2		; A token has three bits.
	jsr	readbits
	beq	next		; No literals.
	cmp	#6
	bcc	literal
	and	#1		; A longer run: four or eight bits.
	tay
	jsr	readtab
	adc	#6
	bcc	literal
	tax			; The escape: a number of runs without a match.
	jsr	readbits
	sta	repeat
	bpl	main		; Always.
literal:
	sta	len
	lda	z:XIPZSRCPTR	; The literals are below the last byte that was read.
	sec
	sbc	len
	sta	z:XIPZSRCPTR
	sta	z:XIPZCOPYPTR
	lda	z:XIPZSRCPTR+1
	sbc	#0
	sta	z:XIPZSRCPTR+1
	sta	z:XIPZCOPYPTR+1
	jsr	copy
next:	ldx	repeat		; Is there a match?
	beq	match
	dec	repeat
	bpl	main		; Always.
match:	jsr	readbits	; X is zero, one bit.
	beq	len_long
	jsr	readtab		; Length two, eight bits of offset.
	ldx	#2
	stx	len
	bcc	matchcopy	; Always.
len_long:
	inx
	jsr	readbits	; One bit.
	beq	len3
	inx
	jsr	readbits	; One bit, selects four or eight bits.
	jsr	readtab
	adc	#1
len3:	adc	#3		; The length is the number plus four, or three.
	sta	len
	inx
	jsr	readbits	; One bit, selects eight or 8+STEP bits.
	iny
	jsr	readtab
matchcopy:
	adc	z:XIPZDSTPTR	; The source is the offset above the destination.
	sta	z:XIPZCOPYPTR
	lda	z:XIPZCOPYPTR+1
	adc	z:XIPZDSTPTR+1
	sta	z:XIPZCOPYPTR+1
	jsr	copy
	beq	main		; Always.
copy:				; Copy len bytes, the destination moves down.
	ldy	len
	sec
	lda	z:XIPZDSTPTR
	sbc	len
	sta	z:XIPZDSTPTR
	bcs	copy_1
	dec	z:XIPZDSTPTR+1
copy_1:	lda	(XIPZCOPYPTR),y
	sta	(XIPZDSTPTR),y
	dey
	bne	copy_1
	rts
readtab:			; X is the width of the number minus one.
	ldx	tab,y
readbits:			; Read X+1 bits into A, the high bits into XIPZCOPYPTR+1.
	lda	#0
	sta	z:XIPZCOPYPTR+1
rb_loop:
	ldy	bitcnt
	beq	refill
rb_bit:	asl	bitbuf
	rol
	rol	z:XIPZCOPYPTR+1
	dec	bitcnt
	dex
	bpl	rb_loop
	tay			; The number is also in Y, X is $ff.
	rts
refill:	sta	tmp		; Get the next byte.
	lda	(XIPZSRCPTR),y	; Y is zero.
	sta	bitbuf
	lda	#8
	sta	bitcnt
	lda	z:XIPZSRCPTR	; The byte below the packed data was fetched: finished.
	cmp	z:XIPZENDPTR
	bne	rf_next
	lda	z:XIPZSRCPTR+1
	cmp	z:XIPZENDPTR+1
	beq	finished
rf_next:
	lda	z:XIPZSRCPTR	; Move the pointer down.
	bne	rf_nb
	dec	z:XIPZSRCPTR+1
rf_nb:	dec	z:XIPZSRCPTR
	lda	tmp
	jmp	rb_bit
finished:
	pla			; Drop the return address of readbits.
	pla
	rts
tab:	.byte	3, 7, 0		; Widths of the numbers minus one, the last is 7+STEP.
repeat:	.byte	0		; Literal runs without a match that are to come.
bitbuf:	.byte	0		; Bits that have not been read, left aligned.
bitcnt:	.byte	0		; Number of these bits.
len:	.byte	0		; Length of a run or a match.
tmp:	.byte	0
.endproc
