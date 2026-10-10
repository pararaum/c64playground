; -*- mode: asm -*-

;;; Decrunch routine for the lzrc cruncher (xipz -a lzrc -r).
;;;
;;; LZ77 with an adaptive binary range coder (the one of squz), see
;;;	lzrc.hh in the xipz sources for a description of the format.
;;;
;;; If the pointers and tables are not defined before including this file
;;;	then the defaults are used: ptr1, ptr2, ptr3 (16 bytes of zero
;;;	page starting at ptr3) and the text screen $0400-$067F for the tables.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, LZ_ZP (16 bytes), the tables
;;;	(LZ_TABLES, $280 bytes, page aligned), X is not preserved
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points somewhere near the end of the compressed data.
;;; Stack: needs about 10 bytes of stack.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	LZ_ZP
LZ_ZP = ptr3
	.endif
	.ifndef	LZ_TABLES
LZ_TABLES = $0400
	.endif

LZ_LIT = LZ_TABLES		; Literal trees, two pages.
LZ_FLAG = LZ_TABLES+$200	; Flag match (two contexts), $202: flag repeat
LZ_EG = LZ_TABLES+$220		; Elias-gamma contexts $20 bytes each: offset, length, repeat length
LZ_RANGE = LZ_ZP		; 16 bit range
LZ_CODE = LZ_ZP+2		; 16 bit code
LZ_BITS = LZ_ZP+4		; bit buffer with sentinel
LZ_PROB = LZ_ZP+5		; pointer to the probabilities
LZ_BOUND = LZ_ZP+7		; 16 bit bound, the multiplier during the multiplication
LZ_OFF = LZ_ZP+9		; 16 bit offset of the last match
LZ_LEN = LZ_ZP+11		; 16 bit length
LZ_SRC = LZ_ZP+13		; 16 bit source of the copy
LZ_PREV = LZ_ZP+15		; previous token was a match


.proc	decrunch_lzrc
	;; Probabilities start at 128.
	ldx	#0
	lda	#128
init:	sta	LZ_TABLES,x
	sta	LZ_TABLES+$100,x
	sta	LZ_TABLES+$200,x
	inx
	bne	init
	stx	z:LZ_PREV
	lda	#$80
	sta	z:LZ_BITS
	lda	#$FF
	sta	z:LZ_RANGE
	sta	z:LZ_RANGE+1
	ldy	#16		; The first 16 bits are the code.
cl:	jsr	getbit
	rol	z:LZ_CODE
	rol	z:LZ_CODE+1
	dey
	bne	cl
	sty	z:LZ_OFF+1	; The offset is one at first.
	iny
	sty	z:LZ_OFF
loop:	lda	#>LZ_FLAG
	sta	z:LZ_PROB+1
	lda	#0
	sta	z:LZ_PROB
	ldy	z:LZ_PREV
	jsr	decbit
	bcs	match
	lda	#>LZ_LIT	; The literal tree of the context.
	clc
	adc	z:LZ_PREV
	sta	z:LZ_PROB+1
	ldy	#1
lloop:	jsr	decbit
	tya
	rol	a
	tay			; The carry is set after the eighth bit.
	bcc	lloop
	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	lit1
	inc	z:XIPZDSTPTR+1
lit1:	sty	z:LZ_PREV	; Y=0
	jmp	loop
match:	ldy	#2		; Flag repeat.
	jsr	decbit
	bcs	rep
	lda	#$20		; Offset.
	jsr	gamma
	lda	z:LZ_LEN
	sta	z:LZ_OFF
	lda	z:LZ_LEN+1
	sta	z:LZ_OFF+1
	lda	#$40		; Length - 1.
	jsr	gamma
	inc	z:LZ_LEN
	bne	copy
	inc	z:LZ_LEN+1
	bne	copy		; Always.
rep:	lda	#$60
	jsr	gamma
copy:	lda	#1
	sta	z:LZ_PREV
	sec
	lda	z:XIPZDSTPTR
	sbc	z:LZ_OFF
	sta	z:LZ_SRC
	lda	z:XIPZDSTPTR+1
	sbc	z:LZ_OFF+1
	sta	z:LZ_SRC+1
	ldy	#0
cp:	lda	(LZ_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	inc	z:LZ_SRC
	bne	cp1
	inc	z:LZ_SRC+1
cp1:	inc	z:XIPZDSTPTR
	bne	cp2
	inc	z:XIPZDSTPTR+1
cp2:	lda	z:LZ_LEN
	bne	cp3
	dec	z:LZ_LEN+1
cp3:	dec	z:LZ_LEN
	lda	z:LZ_LEN
	ora	z:LZ_LEN+1
	bne	cp
	jmp	loop

	;; Next byte of the stream in A. Clobbers X.
getbyte:
	ldx	#0
	lda	(XIPZSRCPTR,x)
	inc	z:XIPZSRCPTR
	bne	gbr
	inc	z:XIPZSRCPTR+1
gbr:	rts

	;; Next bit of the stream in the carry. Clobbers A and X.
getbit:	asl	z:LZ_BITS
	bne	gbb
	jsr	getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZ_BITS
gbb:	rts

	;; Decode one bit with the probability at (LZ_PROB),y. Preserves Y.
decbit:	lda	(LZ_PROB),y
	sta	z:LZ_BOUND
	;; bound = range hi * probability.
	lda	#0
	ldx	#8
	clc
mul0:	bcc	mul1
	clc
	adc	z:LZ_RANGE+1
mul1:	ror	a
	ror	z:LZ_BOUND
	dex
	bpl	mul0
	sta	z:LZ_BOUND+1
	lda	z:LZ_CODE
	sec
	sbc	z:LZ_BOUND
	tax
	lda	z:LZ_CODE+1
	sbc	z:LZ_BOUND+1
	bcc	one
	sta	z:LZ_CODE+1	; Zero: code and range are reduced by the bound.
	stx	z:LZ_CODE
	lda	z:LZ_RANGE
	sbc	z:LZ_BOUND	; Carry is set.
	sta	z:LZ_RANGE
	lda	z:LZ_RANGE+1
	sbc	z:LZ_BOUND+1
	sta	z:LZ_RANGE+1
	clc
	bcc	upd		; Always.
one:	lda	z:LZ_BOUND	; One: the range is the bound.
	sta	z:LZ_RANGE
	lda	z:LZ_BOUND+1
	sta	z:LZ_RANGE+1
	sec
upd:	php
	bcc	down
	lda	(LZ_PROB),y	; p += (256-p+15)>>4, at most 255. The carry is set.
	eor	#$FF
	adc	#15
	ror	a
	lsr	a
	lsr	a
	lsr	a
	clc
	adc	(LZ_PROB),y
	bcc	store
	lda	#$FF
	bne	store		; Always.
down:	lda	(LZ_PROB),y	; p -= (p+15)>>4, at least 1. The carry is clear.
	adc	#15
	ror	a
	lsr	a
	lsr	a
	lsr	a
	eor	#$FF
	sec
	adc	(LZ_PROB),y
	bne	store
	lda	#1
store:	sta	(LZ_PROB),y
renorm:	lda	z:LZ_RANGE+1	; Shift while the range is below $8000.
	bmi	rnd
	asl	z:LZ_RANGE
	rol	z:LZ_RANGE+1
	jsr	getbit
	rol	z:LZ_CODE
	rol	z:LZ_CODE+1
	jmp	renorm
rnd:	plp
	rts
	;; Elias-gamma number in LZ_LEN, the low byte of the context in A.
gamma:	sta	z:LZ_PROB
	ldy	#0
un:	jsr	decbit	; Unary part.
	bcc	unend
	iny
	bne	un		; Always.
unend:	cpy	#16		; End of data.
	bne	g1
	pla
	pla
	rts
g1:	lda	#1
	sta	z:LZ_LEN
	lda	#0
	sta	z:LZ_LEN+1
	tya			; The carry is clear.
	adc	#16
	tay
mant:	cpy	#17
	bcc	gr
	jsr	decbit
	rol	z:LZ_LEN
	rol	z:LZ_LEN+1
	dey
	bne	mant		; Always.
gr:	rts

.endproc
