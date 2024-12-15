; -*- mode: asm -*-

;;; Decrunch routine for the qadz cruncher.
;;;
;;; If the three pointers are not defined before including this file
;;;	then the defaults (ptr1, ptr2, ptr3) are used.
;;;
;;; Input:
;;;	- XIPZSRCPTR: must point to compressed data
;;;	- XIPZDSTPTR: must point to destination address
;;; Modifies: XIPAUXPTR: used during compression
;;; Output: -

	.ifndef	XIPZSRCPTR
	.define XIPZSRCPTR ptr1
	.endif
	.ifndef	XIPZDSTPTR
	.define XIPZDSTPTR ptr2
	.endif
	.ifndef	XIPZAUXPTR
	.define XIPZAUXPTR ptr3
	.endif

.proc	decrunch_qadz
	ldy	#0
	lda	(XIPZSRCPTR),y	; Get next byte.
	bmi	backref		; Back reference and copy.
	bne	literal		; This is a literal run
	rts
literal:
	tax			; Keep number of bytes safe.
	tay			; Put number of bytes into index register.
	inc	XIPZSRCPTR	; increment src
	bne	litcop
	inc	XIPZSRCPTR+1
litcop:	lda	(XIPZSRCPTR),y
	sta	(XIPZDSTPTR),y
	dey
	bpl	litcop
	jsr	incsrc
	jsr	incdst
	jmp	decrunch_qadz
backref:
	eor	#$ff		; Negate A, see below
	tax			; Keep run length safe.
	inx			; see above, negate is eor #$ff, then +1
	iny
	lda	(XIPZSRCPTR),y	; How far to go back?
	sta	@sbc		; Self-modifying trick.
	lda	XIPZDSTPTR	; Subtract from DSTPTR and put into AUXPTR
	sec
	sbc	#00
	@sbc = *-1
	sta	XIPZAUXPTR
	lda	XIPZDSTPTR+1
	sbc	#0
	sta	XIPZAUXPTR+1
	txa
	tay			; Y=X, run length
bckcop:	lda	(XIPZAUXPTR),y
	sta	(XIPZDSTPTR),y
	dey
	bpl	bckcop
	jsr	incdst		; Adjust destination.
	ldx	#2
	jsr	incsrc		; Skip two bytes
	jmp	decrunch_qadz
incsrc:				; increment by X
	txa
	clc
	adc	XIPZSRCPTR
	sta	XIPZSRCPTR
	lda	XIPZSRCPTR+1
	adc	#0
	sta	XIPZSRCPTR+1
	rts
incdst:				; increment by X
	txa
	clc
	adc	XIPZDSTPTR
	sta	XIPZDSTPTR
	lda	XIPZDSTPTR+1
	adc	#0
	sta	XIPZDSTPTR+1
	rts
.endproc
