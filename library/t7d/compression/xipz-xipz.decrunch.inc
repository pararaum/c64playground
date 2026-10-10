; -*- mode: asm -*-

;;; Decrunch routine for the xipz cruncher (xipz -r, the default algorithm).
;;;
;;; If the pointers are not defined before including this file then the
;;;	defaults (ptr1, ptr2, ptr3) are used.
;;;
;;; The raw stream starts with the number of bits n (one byte), followed
;;; by the table of 2^n bytes and the bit stream. The stream has no end
;;; marker, so the end of the output must be given.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;;	- XIPZENDPTR: must point to the address behind the last byte to produce
;;; Modifies: A, X, Y
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

.proc	decrunch_xipz
	ldy	#0
	lda	(XIPZSRCPTR),y	; Number of bits.
	sta	nbits
	tax
	lda	#1
@shift:	asl	a
	dex
	bne	@shift		; A = 2^n, size of the table.
	sta	tabsize
	sec			; Table starts behind the header byte.
	lda	z:XIPZSRCPTR
	adc	#0
	sta	table
	lda	z:XIPZSRCPTR+1
	adc	#0
	sta	table+1
	clc			; Bit stream starts behind the table, X and sourceptr point one byte before.
	lda	z:XIPZSRCPTR
	adc	tabsize
	tax
	lda	z:XIPZSRCPTR+1
	adc	#0
	sta	sourceptr+1
	lda	z:XIPZDSTPTR
	sta	dest
	lda	z:XIPZDSTPTR+1
	sta	dest+1
	lda	#1
	sta	bitcount
	lda	#0
	tay
	beq	getbit		; Always.
first:	tay			; Compare A to zero for test if literal or compressed.
	beq	ldyn		; zero = compressed
	ldy	#8		; Literal, therefore get eight bits.
	.byte	$2c		; Skip the following instruction.
ldyn:	ldy	#0		; Number of bits of a compressed token.
	nbits = *-1
getbit:
	lsr	bitcount	; Update the bit counter (move the single bit).
	bne	skip		; Less than eight bits moved.
	ror	bitcount	; Put the bit in the carry into the MSB (reset counter).
	inx			; Increment source pointer.
	bne	skip		; Overflow?
	inc	sourceptr+1	; Increment high byte.
skip:	asl	a:0,x		; Get next bit from source.
	sourceptr = *-2
	rol			; Move the bit into the accumulator.
	dey			; Decrement the bit counter.
	bmi	first		; Was this the first bit? It will decide if literal or compressed.
	bne	getbit		; All bits moved into A?
	bcs	lit		; bit 9 of .A (is set if literal as we came with carry set)
	tay			; Moved index in compression table.
	lda	a:$0000,y		; Get the value.
	table = *-2
lit:	sta	a:$0000		; Now save the byte.
	dest = *-2
	inc	dest		; Increment destination pointer.
	bne	chk
	inc	dest+1
chk:	lda	dest		; Reached the end?
	cmp	z:XIPZENDPTR
	bne	cont
	lda	dest+1
	cmp	z:XIPZENDPTR+1
	beq	finished
cont:	ldy	#0		; Y must be zero when we reenter the loop.
	tya			; Clear the accumulator so that no left over bits muddy the result.
	jmp	getbit
finished:
	lda	dest
	sta	z:XIPZDSTPTR
	lda	dest+1
	sta	z:XIPZDSTPTR+1
	rts
bitcount:	.byte	1	; Single bit moving through the byte.
tabsize:	.byte	0
.endproc
