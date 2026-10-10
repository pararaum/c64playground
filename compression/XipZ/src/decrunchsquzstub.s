BLOCKX = $0334			; Main part of the decoder: cassette buffer.
BLOCKZ = $0100			; Range decoder: bottom of the stack page, the stack grows down from $01FF.
SQUZ_TABLES = $0400		; Probabilities and dictionary in the text screen.
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
XIPZSRCPTR = $58		; Stream pointer.
XIPZDSTPTR = $26		; Output pointer.
SQUZ_ZP = $5A			; 13 bytes of zero page.

	.include	"t7d/basic.i"
	.export	tocopy
	.export	tocopy_end

	.segment	"EXEHDR"
	.word	thebrk
	.word	770
	.byte	$9e,"2061"
thebrk:	brk
	brk
	brk

	.data
tocopy:
	;; 	.byte	"copy"
tocopy_end:

	.rodata
parameters:
	.export	stubpageLO=*-$801
	.export	stubpageHI=*+1-$801
	.word	ENDADDRESS
	.export stubendofcdata_offset=*-$801
	.word	tocopy_end
	.byte	"t7d"
	.export stubbeginofcdata_offset=*-$801
	.word	tocopy
parameters_end:
	.export	stubparameters_offset=parameters-$801

;;; Decrunch routine for the squz cruncher. This is the code of
;;; library/t7d/compression/xipz-squz.decrunch.inc (keep them in sync, the
;;; labels have a prefix here) in two pieces: the main part, put and oplen
;;; in the cassette buffer and the range decoder at the bottom of the
;;; stack page.
;;; Input: XIPZSRCPTR, XIPZDSTPTR.
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


decrunchdata:
	.org	BLOCKX
	;; Probabilities start at 128.
	ldx	#0
	lda	#128
sq_init:	sta	SQUZ_LIT,x
	sta	SQUZ_LIT+$100,x
	sta	SQUZ_LIT+$200,x
	sta	SQUZ_PAIR,x
	inx
	bne	sq_init
	stx	z:SQUZ_POS
	lda	#$80
	sta	z:SQUZ_BITS
	lda	#$FF
	sta	z:SQUZ_RANGE
	sta	z:SQUZ_RANGE+1
	jsr	sq_getbyte		; Header: the number of pairs.
	sta	z:SQUZ_N
	ldy	#16		; The first 16 bits are the code.
sq_cl:	jsr	sq_getbit
	rol	z:SQUZ_CODE
	rol	z:SQUZ_CODE+1
	dey
	bne	sq_cl
	;; Dictionary: left and right token of every pair.
	sty	z:SQUZ_DI	; Y=0
sq_dict:	lda	z:SQUZ_DI
	lsr	a
	cmp	z:SQUZ_N
	bcs	sq_data
	jsr	sq_dectok
	ldx	z:SQUZ_DI
	sta	SQUZ_V,x
	lda	#0
	rol	a
	sta	SQUZ_F,x
	inc	z:SQUZ_DI
	bne	sq_dict		; Always.
sq_data:	lda	#$80		; Sentinel, a flag which is never used.
	pha
sq_token:	jsr	sq_dectok
	bcc	sq_lit
	cmp	z:SQUZ_N
	beq	sq_done		; End of data.
sq_expand:	asl	a
	tax			; X = 2 * pair index.
	lda	SQUZ_V+1,x	; Right operand is handled later.
	pha
	lda	SQUZ_F+1,x
	pha
	lda	SQUZ_V,x	; Left operand now.
	ldy	SQUZ_F,x
	bne	sq_expand
sq_lit:	jsr	sq_put
sq_pop:	pla
	bmi	sq_fin
	lsr	a		; Carry = flag.
	pla
	bcs	sq_expand
	bcc	sq_lit		; Always.
sq_fin:	pha			; Keep the sentinel.
	jmp	sq_token
sq_done:	pla
	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2

	;; Write the byte in A and advance the position inside the instruction.
sq_put:	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	sq_putp
	inc	z:XIPZDSTPTR+1
sq_putp:	ldx	z:SQUZ_POS
	bne	sq_adv
	jsr	sq_oplen
	sta	z:SQUZ_LEN
sq_adv:	inc	z:SQUZ_POS
	lda	z:SQUZ_POS
	cmp	z:SQUZ_LEN
	bcc	sq_putr
	lda	#0
	sta	z:SQUZ_POS
sq_putr:	rts

	;; Length of the instruction with the opcode in A.
sq_oplen:	tax
	and	#$9F
	bne	sq_opn
	lda	#1		; BRK, RTI, RTS
	cpx	#$20
	bne	sq_opr
	lda	#3		; JSR
sq_opr:	rts
sq_opn:	txa
	and	#$1C
	lsr	a
	sta	z:SQUZ_BOUND
	txa
	and	#1
	ora	z:SQUZ_BOUND
	tay			; Index is 2 * (bits 2-4 of the opcode) + (bit 0)
	lda	sq_lentab,y
	rts
sq_lentab:	.byte	2,2,2,2,1,2,3,3
	.byte	2,2,2,2,1,3,3,3

	len_x = *-BLOCKX
	.reloc
img_z:
	.org	BLOCKZ
	;; Next byte of the stream in A. Clobbers X.
sq_getbyte:
	ldx	#0
	lda	(XIPZSRCPTR,x)
	inc	z:XIPZSRCPTR
	bne	sq_gbr
	inc	z:XIPZSRCPTR+1
sq_gbr:	rts

	;; Next bit of the stream in the carry. Clobbers A and X.
sq_getbit:	asl	z:SQUZ_BITS
	bne	sq_gbb
	jsr	sq_getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:SQUZ_BITS
sq_gbb:	rts

	;; Decode one token. Returns carry = pair flag, A = byte or pair index.
sq_dectok:	lda	z:SQUZ_POS	; Context for the pair trees: $20 bytes each.
	asl	a
	asl	a
	asl	a
	asl	a
	asl	a
	sta	z:SQUZ_PROB
	lda	#>SQUZ_PAIR
	sta	z:SQUZ_PROB+1
	ldy	#0
	jsr	sq_decbit		; Flag.
	bcs	sq_pair
	lda	z:SQUZ_POS	; The literal tree of this context.
	clc
	adc	#>SQUZ_LIT
	sta	z:SQUZ_PROB+1
	lda	#0
	sta	z:SQUZ_PROB
	ldy	#1
sq_lloop:	jsr	sq_decbit
	tya
	rol	a
	tay			; The carry is set after the eighth bit.
	bcc	sq_lloop
	clc			; Literal.
	rts			; A = byte
sq_pair:	ldy	#1
sq_ploop:	jsr	sq_decbit
	tya
	rol	a
	tay
	cpy	#32
	bcc	sq_ploop
	tya
	and	#31
	sec
	rts

	;; Decode one bit with the probability at (SQUZ_PROB),y. Preserves Y.
sq_decbit:	lda	(SQUZ_PROB),y
	sta	z:SQUZ_BOUND
	;; bound = range hi * probability.
	lda	#0
	ldx	#8
	clc
sq_mul0:	bcc	sq_mul1
	clc
	adc	z:SQUZ_RANGE+1
sq_mul1:	ror	a
	ror	z:SQUZ_BOUND
	dex
	bpl	sq_mul0
	sta	z:SQUZ_BOUND+1
	lda	z:SQUZ_CODE
	sec
	sbc	z:SQUZ_BOUND
	tax
	lda	z:SQUZ_CODE+1
	sbc	z:SQUZ_BOUND+1
	bcc	sq_one
	sta	z:SQUZ_CODE+1	; Zero: code and range are reduced by the bound.
	stx	z:SQUZ_CODE
	lda	z:SQUZ_RANGE
	sbc	z:SQUZ_BOUND	; Carry is set.
	sta	z:SQUZ_RANGE
	lda	z:SQUZ_RANGE+1
	sbc	z:SQUZ_BOUND+1
	sta	z:SQUZ_RANGE+1
	clc
	bcc	sq_upd		; Always.
sq_one:	lda	z:SQUZ_BOUND	; One: the range is the bound.
	sta	z:SQUZ_RANGE
	lda	z:SQUZ_BOUND+1
	sta	z:SQUZ_RANGE+1
	sec
sq_upd:	php
	bcc	sq_down
	lda	(SQUZ_PROB),y	; p += (256-p+7)>>3, at most 255. The carry is set.
	eor	#$FF
	adc	#7
	ror	a
	lsr	a
	lsr	a
	clc
	adc	(SQUZ_PROB),y
	bcc	sq_store
	lda	#$FF
	bne	sq_store		; Always.
sq_down:	lda	(SQUZ_PROB),y	; p -= (p+7)>>3, at least 1. The carry is clear.
	adc	#7
	ror	a
	lsr	a
	lsr	a
	eor	#$FF
	sec
	adc	(SQUZ_PROB),y
	bne	sq_store
	lda	#1
sq_store:	sta	(SQUZ_PROB),y
sq_renorm:	lda	z:SQUZ_RANGE+1	; Shift while the range is below $8000.
	bmi	sq_rnd
	asl	z:SQUZ_RANGE
	rol	z:SQUZ_RANGE+1
	jsr	sq_getbit
	rol	z:SQUZ_CODE
	rol	z:SQUZ_CODE+1
	jmp	sq_renorm
sq_rnd:	plp
	rts
	len_z = *-BLOCKZ
	.reloc
decrunchdata_end:

	.export	stubjump_offset = stubjump-BLOCKX+decrunchdata-$801

	.code
_main:				; Must be the first code so that SYS works.
	;; Copy the copy parameters.
	ldx	#parameters_end-parameters-1
@pl:
	lda	parameters,x	; Get the three parameters.
	sta	z:$58,x		; Store them in the ZP.
	dex
	bpl	@pl		; parameter loop
	jsr	MEMORY_MOVE	; Leaves with X=0
	;; Now $58/$59 points to beginning of data-256!
	sei
	;; Copy the two pieces of the decoder, every piece is less than 256 bytes.
	ldx	#len_x
@c1:	lda	decrunchdata-1,x
	sta	a:BLOCKX-1,x
	dex
	bne	@c1
	ldx	#len_z
@c3:	lda	img_z-1,x
	sta	a:BLOCKZ-1,x
	dex
	bne	@c3
	inc	XIPZSRCPTR+1	; Adjust to beginning of compressed data.
	lda	#<DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetLO=*-1-$801
	sta	XIPZDSTPTR
	lda	#>DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetHI=*-1-$801
	sta	XIPZDSTPTR+1
	ldx	#$FF		; The decoder lives below the stack, make it as large as possible.
	txs
	jmp	BLOCKX
