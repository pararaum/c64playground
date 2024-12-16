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
	tax			; Keep run length safe in X.
	inx			; see above, negate is eor #$ff, then +1
	stx	sm_runlen	; Put run length self-modifying in code.
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
	;; Copy upward as we can have longer runs even if data is only partial. E.g. first byte is literal then set AUXPTR to DSTPTR-1 and copy for 100 or so bytes. This is essentially a run-length encoding for free.
	ldy	#0		; Clear index Y, X has number of bytes.
bckcop:	lda	(XIPZAUXPTR),y
	sta	(XIPZDSTPTR),y
	iny			; Now go to next byte.
	dex			; Decrement the counter.
	bpl	bckcop
	lda	#0		; Placeholder for run length.
	sm_runlen = *-1
	jsr	incdstA		; Adjust destination.
	lda	#2
	jsr	incsrcA		; Skip two bytes
	jmp	decrunch_qadz
incsrc:				; increment by X
	txa
incsrcA:
	clc
	adc	XIPZSRCPTR
	sta	XIPZSRCPTR
	lda	XIPZSRCPTR+1
	adc	#0
	sta	XIPZSRCPTR+1
	rts
incdst:				; increment by X
	txa
incdstA:
	clc
	adc	XIPZDSTPTR
	sta	XIPZDSTPTR
	lda	XIPZDSTPTR+1
	adc	#0
	sta	XIPZDSTPTR+1
	rts
.endproc
