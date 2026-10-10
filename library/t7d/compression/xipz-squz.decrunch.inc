; -*- mode: asm -*-

;;; Decrunch routine for the squz cruncher (xipz -a squz -r).
;;;
;;; squz is byte pair encoding with an adaptive binary range coder, see
;;;	squz.hh in the xipz sources for a description of the format.
;;;
;;; If the pointers and tables are not defined before including this file
;;;	then the defaults are used: ptr1, ptr2, ptr3 (13 bytes, ptr3 up to
;;;	the middle of regbank) and the text screen $0400-$07DF for the tables.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, SQUZ_ZP (13 bytes), the tables
;;;	(SQUZ_TABLES, $3E0 bytes, page aligned), X is not preserved
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points somewhere near the end of the compressed data.
;;; Stack: needs about 3 + 2 * 8 bytes of stack.
;;;
;;; The tables are: three literal trees of $100 bytes, then the three
;;;	small trees for the pair flag and the pair index ($20 bytes each),
;;;	then the dictionary (value and flag of the left and right operand,
;;;	interleaved, of up to 32 pairs).

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	SQUZ_ZP
SQUZ_ZP = ptr3
	.endif
	.ifndef	SQUZ_TABLES
SQUZ_TABLES = $0400
	.endif

SQUZ_LIT = SQUZ_TABLES		; Literal trees, three pages.
SQUZ_PAIR = SQUZ_TABLES+$300	; Pair trees, $20 bytes per context.
SQUZ_V = SQUZ_PAIR+$60		; Dictionary: value of the operands, left of pair i at 2*i, right at 2*i+1
SQUZ_F = SQUZ_PAIR+$A0		;	flag of the operands (0 = literal)
SQUZ_RANGE = SQUZ_ZP		; 16 bit range
SQUZ_CODE = SQUZ_ZP+2		; 16 bit code
SQUZ_BITS = SQUZ_ZP+4		; bit buffer with sentinel
SQUZ_PROB = SQUZ_ZP+5		; pointer to the probabilities
SQUZ_BOUND = SQUZ_ZP+7		; 16 bit bound, the multiplier during the multiplication
SQUZ_POS = SQUZ_ZP+9		; position inside the instruction
SQUZ_LEN = SQUZ_ZP+10		; length of the instruction
SQUZ_N = SQUZ_ZP+11		; number of pairs
SQUZ_DI = SQUZ_ZP+12		; dictionary index

.proc	decrunch_squz
	;; Probabilities start at 128.
	ldx	#0
	lda	#128
init:	sta	SQUZ_LIT,x
	sta	SQUZ_LIT+$100,x
	sta	SQUZ_LIT+$200,x
	sta	SQUZ_PAIR,x
	inx
	bne	init
	stx	z:SQUZ_POS
	lda	#$80
	sta	z:SQUZ_BITS
	lda	#$FF
	sta	z:SQUZ_RANGE
	sta	z:SQUZ_RANGE+1
	jsr	getbyte		; Header: the number of pairs.
	sta	z:SQUZ_N
	ldy	#16		; The first 16 bits are the code.
cl:	jsr	getbit
	rol	z:SQUZ_CODE
	rol	z:SQUZ_CODE+1
	dey
	bne	cl
	;; Dictionary: left and right token of every pair.
	sty	z:SQUZ_DI	; Y=0
dict:	lda	z:SQUZ_DI
	lsr	a
	cmp	z:SQUZ_N
	bcs	data
	jsr	dectok
	ldx	z:SQUZ_DI
	sta	SQUZ_V,x
	lda	#0
	rol	a
	sta	SQUZ_F,x
	inc	z:SQUZ_DI
	bne	dict		; Always.
data:	lda	#$80		; Sentinel, a flag which is never used.
	pha
token:	jsr	dectok
	bcc	lit
	cmp	z:SQUZ_N
	beq	done		; End of data.
expand:	asl	a
	tax			; X = 2 * pair index.
	lda	SQUZ_V+1,x	; Right operand is handled later.
	pha
	lda	SQUZ_F+1,x
	pha
	lda	SQUZ_V,x	; Left operand now.
	ldy	SQUZ_F,x
	bne	expand
lit:	jsr	put
pop:	pla
	bmi	fin
	lsr	a		; Carry = flag.
	pla
	bcs	expand
	bcc	lit		; Always.
fin:	pha			; Keep the sentinel.
	jmp	token
done:	pla
	rts

	;; Write the byte in A and advance the position inside the instruction.
put:	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	putp
	inc	z:XIPZDSTPTR+1
putp:	ldx	z:SQUZ_POS
	bne	adv
	jsr	oplen
	sta	z:SQUZ_LEN
adv:	inc	z:SQUZ_POS
	lda	z:SQUZ_POS
	cmp	z:SQUZ_LEN
	bcc	putr
	lda	#0
	sta	z:SQUZ_POS
putr:	rts

	;; Length of the instruction with the opcode in A.
oplen:	tax
	and	#$9F
	bne	opn
	lda	#1		; BRK, RTI, RTS
	cpx	#$20
	bne	opr
	lda	#3		; JSR
opr:	rts
opn:	txa
	and	#$1C
	lsr	a
	sta	z:SQUZ_BOUND
	txa
	and	#1
	ora	z:SQUZ_BOUND
	tay			; Index is 2 * (bits 2-4 of the opcode) + (bit 0)
	lda	lentab,y
	rts
lentab:	.byte	2,2,2,2,1,2,3,3
	.byte	2,2,2,2,1,3,3,3

	;; Next byte of the stream in A. Clobbers X.
getbyte:
	ldx	#0
	lda	(XIPZSRCPTR,x)
	inc	z:XIPZSRCPTR
	bne	gbr
	inc	z:XIPZSRCPTR+1
gbr:	rts

	;; Next bit of the stream in the carry. Clobbers A and X.
getbit:	asl	z:SQUZ_BITS
	bne	gbb
	jsr	getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:SQUZ_BITS
gbb:	rts

	;; Decode one token. Returns carry = pair flag, A = byte or pair index.
dectok:	lda	z:SQUZ_POS	; Context for the pair trees: $20 bytes each.
	asl	a
	asl	a
	asl	a
	asl	a
	asl	a
	sta	z:SQUZ_PROB
	lda	#>SQUZ_PAIR
	sta	z:SQUZ_PROB+1
	ldy	#0
	jsr	decbit		; Flag.
	bcs	pair
	lda	z:SQUZ_POS	; The literal tree of this context.
	clc
	adc	#>SQUZ_LIT
	sta	z:SQUZ_PROB+1
	lda	#0
	sta	z:SQUZ_PROB
	ldy	#1
lloop:	jsr	decbit
	tya
	rol	a
	tay			; The carry is set after the eighth bit.
	bcc	lloop
	clc			; Literal.
	rts			; A = byte
pair:	ldy	#1
ploop:	jsr	decbit
	tya
	rol	a
	tay
	cpy	#32
	bcc	ploop
	tya
	and	#31
	sec
	rts

	;; Decode one bit with the probability at (SQUZ_PROB),y. Preserves Y.
decbit:	lda	(SQUZ_PROB),y
	sta	z:SQUZ_BOUND
	;; bound = range hi * probability.
	lda	#0
	ldx	#8
	clc
mul0:	bcc	mul1
	clc
	adc	z:SQUZ_RANGE+1
mul1:	ror	a
	ror	z:SQUZ_BOUND
	dex
	bpl	mul0
	sta	z:SQUZ_BOUND+1
	lda	z:SQUZ_CODE
	sec
	sbc	z:SQUZ_BOUND
	tax
	lda	z:SQUZ_CODE+1
	sbc	z:SQUZ_BOUND+1
	bcc	one
	sta	z:SQUZ_CODE+1	; Zero: code and range are reduced by the bound.
	stx	z:SQUZ_CODE
	lda	z:SQUZ_RANGE
	sbc	z:SQUZ_BOUND	; Carry is set.
	sta	z:SQUZ_RANGE
	lda	z:SQUZ_RANGE+1
	sbc	z:SQUZ_BOUND+1
	sta	z:SQUZ_RANGE+1
	clc
	bcc	upd		; Always.
one:	lda	z:SQUZ_BOUND	; One: the range is the bound.
	sta	z:SQUZ_RANGE
	lda	z:SQUZ_BOUND+1
	sta	z:SQUZ_RANGE+1
	sec
upd:	php
	bcc	down
	lda	(SQUZ_PROB),y	; p += (256-p+7)>>3, at most 255. The carry is set.
	eor	#$FF
	adc	#7
	ror	a
	lsr	a
	lsr	a
	clc
	adc	(SQUZ_PROB),y
	bcc	store
	lda	#$FF
	bne	store		; Always.
down:	lda	(SQUZ_PROB),y	; p -= (p+7)>>3, at least 1. The carry is clear.
	adc	#7
	ror	a
	lsr	a
	lsr	a
	eor	#$FF
	sec
	adc	(SQUZ_PROB),y
	bne	store
	lda	#1
store:	sta	(SQUZ_PROB),y
renorm:	lda	z:SQUZ_RANGE+1	; Shift while the range is below $8000.
	bmi	rnd
	asl	z:SQUZ_RANGE
	rol	z:SQUZ_RANGE+1
	jsr	getbit
	rol	z:SQUZ_CODE
	rol	z:SQUZ_CODE+1
	jmp	renorm
rnd:	plp
	rts
.endproc
