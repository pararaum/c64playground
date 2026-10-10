BLOCKX = $0334			; The decoder: cassette buffer.
LZH_HIST = $0400		; History in the text screen.
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
XIPZSRCPTR = $58		; Stream pointer.
XIPZDSTPTR = $26		; Output pointer.
LZH_ZP = $5A			; 6 bytes of zero page.

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


;;; Decrunch routine for the lzh16 cruncher. This is the code of
;;; library/t7d/compression/xipz-lzh16.decrunch.inc (keep them in sync, the
;;; labels have a prefix here), see lzh16.hh for the format.
;;; Input: XIPZSRCPTR, XIPZDSTPTR.
LZH_HLO = LZH_HIST		; History: low bytes of the distances - 1,
LZH_HHI = LZH_HIST+16		;	high bytes.
LZH_SRC = LZH_ZP		; 16 bit source of the copy
LZH_BITS = LZH_ZP+2		; bit buffer with sentinel
LZH_VAL = LZH_ZP+3		; number being read, length - 1 after the gamma code
LZH_NEXT = LZH_ZP+4		; next history slot to be written

decrunchdata:
	.org	BLOCKX
	ldx	#31
	lda	#0
lh_init:	sta	LZH_HIST,x
	dex
	bpl	lh_init
	sta	z:LZH_NEXT
	lda	#$80
	sta	z:LZH_BITS
lh_loop:	jsr	lh_getbit
	bcs	lh_match
	ldx	#8		; Literal.
	jsr	lh_getbits
	ldy	#0
	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	lh_loop
	inc	z:XIPZDSTPTR+1
	bne	lh_loop		; Always.
lh_match:	jsr	lh_getbit
	bcc	lh_new
	ldx	#4		; History slot.
	jsr	lh_getbits
	tax
lh_len:	jsr	lh_gamma	; X is not changed from here on.
	lda	z:XIPZDSTPTR	; Source = destination - distance.
	clc
	sbc	LZH_HLO,x
	sta	z:LZH_SRC
	lda	z:XIPZDSTPTR+1
	sbc	LZH_HHI,x
	sta	z:LZH_SRC+1
	ldy	#0
lh_cp:	lda	(LZH_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	cpy	z:LZH_VAL
	iny
	bcc	lh_cp
	sec			; Y may be 256, add length - 1 + 1.
	lda	z:XIPZDSTPTR
	adc	z:LZH_VAL
	sta	z:XIPZDSTPTR
	bcc	lh_loop
	inc	z:XIPZDSTPTR+1
	bne	lh_loop		; Always.
lh_new:	jsr	lh_gamma	; High byte + 1 of the distance.
	bcs	lh_done		; Overflow: end of the data.
	dec	z:LZH_VAL
	lda	z:LZH_VAL
	pha
	ldx	#8
	jsr	lh_getbits	; Low byte of the distance.
	ldx	z:LZH_NEXT
	sta	LZH_HLO,x
	pla
	sta	LZH_HHI,x
	inc	z:LZH_NEXT
	lda	z:LZH_NEXT
	and	#15
	sta	z:LZH_NEXT
	jmp	lh_len
lh_done:	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2

	;; Interleaved Elias-gamma number in LZH_VAL, carry is set if it
	;; overflows. Does not change X.
lh_gamma:	lda	#1
	sta	z:LZH_VAL
lh_g1:	jsr	lh_getbit
	bcc	lh_gr
	jsr	lh_getbit
	rol	z:LZH_VAL
	bcc	lh_g1
lh_gr:	rts

	;; X bits into A, msb first. X is zero afterwards.
lh_getbits:	lda	#0
	sta	z:LZH_VAL
lh_gb:	jsr	lh_getbit
	rol	z:LZH_VAL
	dex
	bne	lh_gb
	lda	z:LZH_VAL
	rts

	;; Next bit into the carry, changes A and Y.
lh_getbit:	asl	z:LZH_BITS
	bne	lh_gbr
	ldy	#0
	lda	(XIPZSRCPTR),y
	inc	z:XIPZSRCPTR
	bne	lh_gbn
	inc	z:XIPZSRCPTR+1
lh_gbn:	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZH_BITS
lh_gbr:	rts

	len_x = *-BLOCKX
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
	ldx	#len_x
@c1:	lda	decrunchdata-1,x
	sta	a:BLOCKX-1,x
	dex
	bne	@c1
	inc	XIPZSRCPTR+1	; Adjust to beginning of compressed data.
	lda	#<DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetLO=*-1-$801
	sta	XIPZDSTPTR
	lda	#>DEFAULTDESTINATIONADDR
	.export	stubdestination_offsetHI=*-1-$801
	sta	XIPZDSTPTR+1
	ldx	#$FF
	txs
	jmp	BLOCKX
