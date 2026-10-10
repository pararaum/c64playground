; -*- mode: asm -*-

;;; Decrunch routine for the lzw cruncher (xipz -a lzw -r).
;;;
;;; LZW with nine bit codes whose dictionary points into the output, see
;;;	lzw.hh in the xipz sources for a description of the format.
;;;
;;; If the pointers and tables are not defined before including this file
;;;	then the defaults are used: ptr1, ptr2, ptr3 (9 bytes) and the text
;;;	screen $0400-$07FF for the tables.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, LZW_ZP (9 bytes), the tables
;;;	(LZW_TABLES, $400 bytes, page aligned), X is not preserved
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points behind the compressed data.
;;; Stack: needs 4 bytes of stack.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	LZW_ZP
LZW_ZP = ptr3
	.endif
	.ifndef	LZW_TABLES
LZW_TABLES = $0400
	.endif

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

.proc	decrunch_lzw
	ldx	#0
init:	txa
	sta	LZW_ID,x
	inx
	bne	init
	stx	z:LZW_NEXT
	stx	z:LZW_PL
	lda	#$80
	sta	z:LZW_BITS
loop:	jsr	getcode		; A = low byte, carry = ninth bit.
	bcs	dyn
	sta	z:LZW_SRC	; Literal: copy from the identity table.
	lda	#>LZW_ID
	sta	z:LZW_SRC+1
	lda	#1
	sta	z:LZW_LEN
	jsr	add
	jmp	copy
dyn:	cmp	#LZW_NENT
	bcs	special
	sta	z:LZW_CODE
	jsr	add		; The entry may be the one just added.
	ldx	z:LZW_CODE
	lda	LZW_TLO,x
	sta	z:LZW_SRC
	lda	LZW_THI,x
	sta	z:LZW_SRC+1
	lda	LZW_TLEN,x
	sta	z:LZW_LEN
copy:	lda	z:XIPZDSTPTR	; This string is the previous one for the next code.
	sta	z:LZW_PS
	lda	z:XIPZDSTPTR+1
	sta	z:LZW_PS+1
	lda	z:LZW_LEN
	sta	z:LZW_PL
	ldy	#0
cp:	lda	(LZW_SRC),y	; Forward copy, the areas may overlap.
	sta	(XIPZDSTPTR),y
	iny
	cpy	z:LZW_LEN
	bne	cp
	tya
	clc
	adc	z:XIPZDSTPTR
	sta	z:XIPZDSTPTR
	bcc	loop
	inc	z:XIPZDSTPTR+1
	bne	loop		; Always.
special:	cmp	#255
	beq	done
	ldx	#0		; Clear: forget the dictionary.
	stx	z:LZW_NEXT
	stx	z:LZW_PL
	beq	loop		; Always.
done:	rts

	;; New entry: previous string plus the first byte of the current one.
add:	lda	z:LZW_PL
	beq	addr
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
	bcc	adds
	ldx	#0
adds:	stx	z:LZW_NEXT
addr:	rts

	;; Next code: A = low byte, carry = ninth bit. Eight codes share
	;; one byte with their ninth bits, it comes first.
getcode:	ldy	#0
	asl	z:LZW_BITS
	bne	gc
	jsr	getbyte
	rol	a		; The carry is one, becomes the sentinel.
	sta	z:LZW_BITS
gc:
getbyte:	lda	(XIPZSRCPTR),y	; Carry is preserved.
	inc	z:XIPZSRCPTR
	bne	gbr
	inc	z:XIPZSRCPTR+1
gbr:	rts
.endproc
