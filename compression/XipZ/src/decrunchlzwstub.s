BLOCKX = $0334			; The decoder: cassette buffer.
LZW_TABLES = $0400		; Dictionary in the text screen.
DEFAULTDESTINATIONADDR = $33c
ENDADDRESS = $1000

;;; Remember that $58 contains the last byte copied by the basic routine - 256.
XIPZSRCPTR = $58		; Stream pointer.
XIPZDSTPTR = $26		; Output pointer.
LZW_ZP = $5A			; 10 bytes of zero page.

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

;;; Decrunch routine for the lzw cruncher. This is the code of
;;; library/t7d/compression/xipz-lzw.decrunch.inc (keep them in sync, the
;;; labels have a prefix here), see lzw.hh for the format.
;;; Input: XIPZSRCPTR, XIPZDSTPTR.
LZW_TLO = LZW_TABLES		; Dictionary: address of the string,
LZW_THI = LZW_TABLES+$100	;	high byte of the address,
LZW_TLEN = LZW_TABLES+$200	;	length of the string.
LZW_ID = LZW_TABLES+$300	; Identity table, the source of the literals.
LZW_NENT = 254			; Entries; code 254 clears, 255 ends.
LZW_SRC = LZW_ZP		; 16 bit source of the copy
LZW_LEN = LZW_ZP+2		; length of the copy
LZW_PS = LZW_ZP+3		; 16 bit start of the previous string
LZW_PL = LZW_ZP+5		; length of the previous string, 0 = none
LZW_NEXT = LZW_ZP+6		; next entry to be written
LZW_BITS = LZW_ZP+7		; bit buffer with sentinel
LZW_CODE = LZW_ZP+8		; dictionary index

decrunchdata:
	.org	BLOCKX
	ldx	#0
lz_init:	txa
	sta	LZW_ID,x
	inx
	bne	lz_init
	stx	z:LZW_NEXT
	stx	z:LZW_PL
	lda	#$80
	sta	z:LZW_BITS
lz_loop:	jsr	lz_getcode	; A = low byte, carry = ninth bit.
	bcs	lz_dyn
	sta	z:LZW_SRC	; Literal: copy from the identity table.
	lda	#>LZW_ID
	sta	z:LZW_SRC+1
	lda	#1
	sta	z:LZW_LEN
	jsr	lz_add
	jmp	lz_copy
lz_dyn:	cmp	#LZW_NENT
	bcs	lz_special
	sta	z:LZW_CODE
	jsr	lz_add		; The entry may be the one just added.
	ldx	z:LZW_CODE
	lda	LZW_TLO,x
	sta	z:LZW_SRC
	lda	LZW_THI,x
	sta	z:LZW_SRC+1
	lda	LZW_TLEN,x
	sta	z:LZW_LEN
lz_copy:	lda	z:XIPZDSTPTR	; This string is the previous one for the next code.
	sta	z:LZW_PS
	lda	z:XIPZDSTPTR+1
	sta	z:LZW_PS+1
	lda	z:LZW_LEN
	sta	z:LZW_PL
	ldy	#0
lz_cp:	lda	(LZW_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	iny
	cpy	z:LZW_LEN
	bne	lz_cp
	tya
	clc
	adc	z:XIPZDSTPTR
	sta	z:XIPZDSTPTR
	bcc	lz_loop
	inc	z:XIPZDSTPTR+1
	bne	lz_loop		; Always.
lz_special:	cmp	#255
	beq	lz_done
	ldx	#0		; Clear: forget the dictionary.
	stx	z:LZW_NEXT
	stx	z:LZW_PL
	beq	lz_loop		; Always.
lz_done:	jmp	DEFAULTDESTINATIONADDR
	stubjump = *-2

	;; New entry: previous string plus the first byte of the current one.
lz_add:	lda	z:LZW_PL
	beq	lz_addr
	ldx	z:LZW_NEXT
	clc
	adc	#1
	sta	LZW_TLEN,x
	lda	z:LZW_PS
	sta	LZW_TLO,x
	lda	z:LZW_PS+1
	sta	LZW_THI,x
	inx
	cpx	#LZW_NENT
	bcc	lz_adds
	ldx	#0
lz_adds:	stx	z:LZW_NEXT
lz_addr:	rts

	;; Next code: A = low byte, carry = ninth bit. Eight codes share
	;; one byte with their ninth bits, it comes first.
lz_getcode:	ldy	#0
	asl	z:LZW_BITS
	bne	lz_gc
	jsr	lz_getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZW_BITS
lz_gc:
lz_getbyte:	lda	(XIPZSRCPTR),y
	inc	z:XIPZSRCPTR
	bne	lz_gbr
	inc	z:XIPZSRCPTR+1
lz_gbr:	rts

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
