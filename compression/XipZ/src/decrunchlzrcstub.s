BLOCKX = $0334			; Main part of the decoder: cassette buffer.
BLOCKZ = $0100			; Range decoder: bottom of the stack page, the stack grows down from $01FF.
LZ_TABLES = $0400		; Probabilities in the text screen.
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
XIPZSRCPTR = $58		; Stream pointer.
XIPZDSTPTR = $26		; Output pointer.
LZ_ZP = $5A			; 16 bytes of zero page.

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

;;; Decrunch routine for the lzrc cruncher. This is the code of
;;; library/t7d/compression/xipz-lzrc.decrunch.inc (keep them in sync, the
;;; labels have a prefix here), see lzrc.hh for the format. The range
;;; decoder is the one of squz. It is in two pieces: the main part in the
;;; cassette buffer and the range decoder at the bottom of the stack page.
;;; Input: XIPZSRCPTR, XIPZDSTPTR.
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

decrunchdata:
	.org	BLOCKX
	;; Probabilities start at 128.
	ldx	#0
	lda	#128
lz_init:	sta	LZ_TABLES,x
	sta	LZ_TABLES+$100,x
	sta	LZ_TABLES+$200,x
	inx
	bne	lz_init
	stx	z:LZ_PREV
	lda	#$80
	sta	z:LZ_BITS
	lda	#$FF
	sta	z:LZ_RANGE
	sta	z:LZ_RANGE+1
	ldy	#16		; The first 16 bits are the code.
lz_cl:	jsr	lz_getbit
	rol	z:LZ_CODE
	rol	z:LZ_CODE+1
	dey
	bne	lz_cl
	sty	z:LZ_OFF+1	; The offset is one at first.
	iny
	sty	z:LZ_OFF
lz_loop:	lda	#>LZ_FLAG
	sta	z:LZ_PROB+1
	lda	#0
	sta	z:LZ_PROB
	ldy	z:LZ_PREV
	jsr	lz_decbit
	bcs	lz_match
	lda	#>LZ_LIT	; The literal tree of the context.
	clc
	adc	z:LZ_PREV
	sta	z:LZ_PROB+1
	ldy	#1
lz_lloop:	jsr	lz_decbit
	tya
	rol	a
	tay			; The carry is set after the eighth bit.
	bcc	lz_lloop
	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	lz_lit1
	inc	z:XIPZDSTPTR+1
lz_lit1:	sty	z:LZ_PREV	; Y=0
	jmp	lz_loop
lz_match:	ldy	#2		; Flag repeat.
	jsr	lz_decbit
	bcs	lz_rep
	lda	#$20		; Offset.
	jsr	lz_gamma
	lda	z:LZ_LEN
	sta	z:LZ_OFF
	lda	z:LZ_LEN+1
	sta	z:LZ_OFF+1
	lda	#$40		; Length - 1.
	jsr	lz_gamma
	inc	z:LZ_LEN
	bne	lz_copy
	inc	z:LZ_LEN+1
	bne	lz_copy		; Always.
lz_rep:	lda	#$60
	jsr	lz_gamma
lz_copy:	lda	#1
	sta	z:LZ_PREV
	sec
	lda	z:XIPZDSTPTR
	sbc	z:LZ_OFF
	sta	z:LZ_SRC
	lda	z:XIPZDSTPTR+1
	sbc	z:LZ_OFF+1
	sta	z:LZ_SRC+1
	ldy	#0
lz_cp:	lda	(LZ_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	inc	z:LZ_SRC
	bne	lz_cp1
	inc	z:LZ_SRC+1
lz_cp1:	inc	z:XIPZDSTPTR
	bne	lz_cp2
	inc	z:XIPZDSTPTR+1
lz_cp2:	lda	z:LZ_LEN
	bne	lz_cp3
	dec	z:LZ_LEN+1
lz_cp3:	dec	z:LZ_LEN
	lda	z:LZ_LEN
	ora	z:LZ_LEN+1
	bne	lz_cp
	jmp	lz_loop

	len_x = *-BLOCKX
	.reloc
img_z:
	.org	BLOCKZ
	;; Next byte of the stream in A. Clobbers X.
lz_getbyte:
	ldx	#0
	lda	(XIPZSRCPTR,x)
	inc	z:XIPZSRCPTR
	bne	lz_gbr
	inc	z:XIPZSRCPTR+1
lz_gbr:	rts

	;; Next bit of the stream in the carry. Clobbers A and X.
lz_getbit:	asl	z:LZ_BITS
	bne	lz_gbb
	jsr	lz_getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZ_BITS
lz_gbb:	rts

	;; Decode one bit with the probability at (LZ_PROB),y. Preserves Y.
lz_decbit:	lda	(LZ_PROB),y
	sta	z:LZ_BOUND
	;; bound = range hi * probability.
	lda	#0
	ldx	#8
	clc
lz_mul0:	bcc	lz_mul1
	clc
	adc	z:LZ_RANGE+1
lz_mul1:	ror	a
	ror	z:LZ_BOUND
	dex
	bpl	lz_mul0
	sta	z:LZ_BOUND+1
	lda	z:LZ_CODE
	sec
	sbc	z:LZ_BOUND
	tax
	lda	z:LZ_CODE+1
	sbc	z:LZ_BOUND+1
	bcc	lz_one
	sta	z:LZ_CODE+1	; Zero: code and range are reduced by the bound.
	stx	z:LZ_CODE
	lda	z:LZ_RANGE
	sbc	z:LZ_BOUND	; Carry is set.
	sta	z:LZ_RANGE
	lda	z:LZ_RANGE+1
	sbc	z:LZ_BOUND+1
	sta	z:LZ_RANGE+1
	clc
	bcc	lz_upd		; Always.
lz_one:	lda	z:LZ_BOUND	; One: the range is the bound.
	sta	z:LZ_RANGE
	lda	z:LZ_BOUND+1
	sta	z:LZ_RANGE+1
	sec
lz_upd:	php
	bcc	lz_down
	lda	(LZ_PROB),y	; p += (256-p+15)>>4, at most 255. The carry is set.
	eor	#$FF
	adc	#15
	ror	a
	lsr	a
	lsr	a
	lsr	a
	clc
	adc	(LZ_PROB),y
	bcc	lz_store
	lda	#$FF
	bne	lz_store		; Always.
lz_down:	lda	(LZ_PROB),y	; p -= (p+15)>>4, at least 1. The carry is clear.
	adc	#15
	ror	a
	lsr	a
	lsr	a
	lsr	a
	eor	#$FF
	sec
	adc	(LZ_PROB),y
	bne	lz_store
	lda	#1
lz_store:	sta	(LZ_PROB),y
lz_renorm:	lda	z:LZ_RANGE+1	; Shift while the range is below $8000.
	bmi	lz_rnd
	asl	z:LZ_RANGE
	rol	z:LZ_RANGE+1
	jsr	lz_getbit
	rol	z:LZ_CODE
	rol	z:LZ_CODE+1
	jmp	lz_renorm
lz_rnd:	plp
	rts
	;; Elias-gamma number in LZ_LEN, the low byte of the context in A.
lz_gamma:	sta	z:LZ_PROB
	ldy	#0
lz_un:	jsr	lz_decbit	; Unary part.
	bcc	lz_unend
	iny
	bne	lz_un		; Always.
lz_unend:	cpy	#16		; End of data.
	bne	lz_g1
	pla
	pla
	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2
lz_g1:	lda	#1
	sta	z:LZ_LEN
	lda	#0
	sta	z:LZ_LEN+1
	tya			; The carry is clear.
	adc	#16
	tay
lz_mant:	cpy	#17
	bcc	lz_gr
	jsr	lz_decbit
	rol	z:LZ_LEN
	rol	z:LZ_LEN+1
	dey
	bne	lz_mant		; Always.
lz_gr:	rts

	len_z = *-BLOCKZ
	.reloc
decrunchdata_end:

	.export	stubjump_offset = stubjump-BLOCKZ+img_z-$801

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
