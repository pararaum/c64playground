; -*- mode: asm -*-

;;; Decrunch routine for the lzh16 cruncher (xipz -a lzh16 -r).
;;;
;;; LZ77 with a bit stream and a history of 16 offsets, see lzh16.hh in
;;;	the xipz sources for a description of the format.
;;;
;;; If the pointers and tables are not defined before including this file
;;;	then the defaults are used: ptr1, ptr2, ptr3 (5 bytes) and the text
;;;	screen $0400-$041F for the history.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, LZH_ZP (5 bytes), the history
;;;	(LZH_HIST, 32 bytes), X is not preserved
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points behind the compressed data.
;;; Stack: needs 5 bytes of stack.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	LZH_ZP
LZH_ZP = ptr3
	.endif
	.ifndef	LZH_HIST
LZH_HIST = $0400
	.endif

LZH_HLO = LZH_HIST		; History: low bytes of the distances - 1,
LZH_HHI = LZH_HIST+16		;	high bytes.
LZH_SRC = LZH_ZP		; 16 bit source of the copy
LZH_BITS = LZH_ZP+2		; bit buffer with sentinel
LZH_VAL = LZH_ZP+3		; number being read, length - 1 after the gamma code
LZH_NEXT = LZH_ZP+4		; next history slot to be written

.proc	decrunch_lzh16
	ldx	#31
	lda	#0
init:	sta	LZH_HIST,x
	dex
	bpl	init
	sta	z:LZH_NEXT
	lda	#$80
	sta	z:LZH_BITS
loop:	jsr	getbit
	bcs	match
	ldx	#8		; Literal.
	jsr	getbits
	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	loop
	inc	z:XIPZDSTPTR+1
	bne	loop		; Always.
match:	jsr	getbit
	bcc	new
	ldx	#4		; History slot.
	jsr	getbits
	tax
len:	jsr	gamma	; X is not changed from here on.
	lda	z:XIPZDSTPTR	; Source = destination - distance.
	clc
	sbc	LZH_HLO,x
	sta	z:LZH_SRC
	lda	z:XIPZDSTPTR+1
	sbc	LZH_HHI,x
	sta	z:LZH_SRC+1
	ldy	#0
cp:	lda	(LZH_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	cpy	z:LZH_VAL
	iny
	bcc	cp
	sec			; Y may be 256, add length - 1 + 1.
	lda	z:XIPZDSTPTR
	adc	z:LZH_VAL
	sta	z:XIPZDSTPTR
	bcc	loop
	inc	z:XIPZDSTPTR+1
	bne	loop		; Always.
new:	jsr	gamma	; High byte + 1 of the distance.
	bcs	done		; Overflow: end of the data.
	dec	z:LZH_VAL
	lda	z:LZH_VAL
	pha
	ldx	#8
	jsr	getbits	; Low byte of the distance.
	ldx	z:LZH_NEXT
	sta	LZH_HLO,x
	pla
	sta	LZH_HHI,x
	inc	z:LZH_NEXT
	lda	z:LZH_NEXT
	and	#15
	sta	z:LZH_NEXT
	jmp	len
done:	rts

	;; Interleaved Elias-gamma number in LZH_VAL, carry is set if it
	;; overflows. Does not change X.
gamma:	lda	#1
	sta	z:LZH_VAL
g1:	jsr	getbit
	bcc	gr
	jsr	getbit
	rol	z:LZH_VAL
	bcc	g1
gr:	rts

	;; X bits into A, msb first. X is zero afterwards.
getbits:	lda	#0
	sta	z:LZH_VAL
gb:	jsr	getbit
	rol	z:LZH_VAL
	dex
	bne	gb
	lda	z:LZH_VAL
	rts

	;; Next bit into the carry, changes A and Y.
getbit:	asl	z:LZH_BITS
	bne	gbr
	ldy	#0
	lda	(XIPZSRCPTR),y
	inc	z:XIPZSRCPTR
	bne	gbn
	inc	z:XIPZSRCPTR+1
gbn:	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZH_BITS
gbr:	rts
.endproc
