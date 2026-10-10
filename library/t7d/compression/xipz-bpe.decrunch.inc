; -*- mode: asm -*-

;;; Decrunch routine for the bpe cruncher (xipz -a bpe -r).
;;;
;;; If the pointers and the tables are not defined before including
;;;	this file then the defaults are used: ptr1, ptr2, ptr3 (two
;;;	bytes) and the text screen $0400-$05FF for the tables.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: A, X, Y, XIPZSRCPTR, XIPZDSTPTR, XIPZBPEVARS (two bytes:
;;;	escape byte and end byte), the tables XIPZBPELEFT and
;;;	XIPZBPERIGHT (256 bytes each).
;;; Output: XIPZDSTPTR points behind the last decrunched byte, XIPZSRCPTR
;;;	points to the byte after the end marker.
;;; Stack: needs up to 3 + 64 bytes of stack. The tables must not
;;;	overlap the destination.
;;;
;;; Format: the escape byte ESC, the end byte END, the pair table: 32
;;;	bitmap bytes (bit b of byte i marks the pair code 8*i+b), each
;;;	followed by the (left, right) bytes of the codes it marks. Then
;;;	the tokens follow: a literal token (a value which is
;;;	not a pair code) is written as is, a pair token is expanded
;;;	recursively to its left and right token. ESC is followed by a
;;;	raw byte which is written as is. ESC END ends the stream.

	.ifndef	XIPZSRCPTR
XIPZSRCPTR = ptr1
	.endif
	.ifndef	XIPZDSTPTR
XIPZDSTPTR = ptr2
	.endif
	.ifndef	XIPZBPEVARS
XIPZBPEVARS = ptr3
	.endif
	.ifndef	XIPZBPELEFT
XIPZBPELEFT = $0400
	.endif
	.ifndef	XIPZBPERIGHT
XIPZBPERIGHT = XIPZBPELEFT+$100
	.endif
XIPZBPEESC = XIPZBPEVARS
XIPZBPEEND = XIPZBPEVARS+1

.proc	decrunch_bpe
	ldx	#0
init:	txa			; Every token is a literal.
	sta	XIPZBPELEFT,x
	sta	XIPZBPERIGHT,x
	inx
	bne	init
	ldy	#0		; Y=0 from now on.
	jsr	getbyte
	sta	z:XIPZBPEESC
	pha			; Sentinel.
	jsr	getbyte
	sta	z:XIPZBPEEND
	ldx	#0		; X is the code, counts up to 256.
bmap:	jsr	getbyte		; Bitmap byte.
	sec
	ror	a		; C = bit 0, a one marks the end.
nextbit:	bcc	skip
	pha
	jsr	getbyte
	sta	XIPZBPELEFT,x
	jsr	getbyte
	sta	XIPZBPERIGHT,x
	pla
skip:	inx
	lsr	a
	bne	nextbit
	txa
	bne	bmap		; X=0 after 256 codes.
main:	jsr	getbyte
	cmp	z:XIPZBPEESC
	beq	esc
expand:	tax
	cmp	XIPZBPERIGHT,x	; Literal?
	beq	leaf
	lda	XIPZBPERIGHT,x
	pha			; Right half later.
	lda	XIPZBPELEFT,x	; Left half now.
	jmp	expand
leaf:	jsr	put
	pla
	cmp	z:XIPZBPEESC	; Sentinel?
	bne	expand
	pha
	beq	main		; Always.
esc:	jsr	getbyte
	cmp	z:XIPZBPEEND
	beq	done
	jsr	put
	jmp	main
done:	pla			; Remove the sentinel.
	rts
getbyte:
	lda	(XIPZSRCPTR),y
	inc	z:XIPZSRCPTR
	bne	:+
	inc	z:XIPZSRCPTR+1
:	rts
put:	sta	(XIPZDSTPTR),y
	inc	z:XIPZDSTPTR
	bne	:+
	inc	z:XIPZDSTPTR+1
:	rts
.endproc
