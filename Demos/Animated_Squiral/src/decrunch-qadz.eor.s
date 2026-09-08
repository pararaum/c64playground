	.include	"t7d/basic.i"
	.include	"LAMAlib-macros16.inc"
	.include	"globals.i"

	.export	decrunch_to_E000

	.zeropage
SRCPTR:		.res	2
DSTPTR:		.res	2

	.code
.proc	rolling_buffer_update
	sta	Code_rollingbuffer
	PTR=*-2
	inc	PTR		; Increment rolling buffer pointer LO. This wraps after 256 Bytes.
	rts
.endproc

;;; Decrunch routine for the qadz cruncher.
;;; Input: SRCPTR
.proc	decrunch_to_E000
	sta	SRCPTR			; LO SRCPTR
	stx	SRCPTR+1		; HI SRCPTR
	ldax	#GFX_bitmapaddr
	stax	DSTPTR
	lda	#0
	sta	rolling_buffer_update::PTR
decrunch:
	ldy	#0
	lda	(SRCPTR),y	; Get next byte.
	bmi	backref		; Back reference and copy.
	bne	literal		; This is a literal run
	rts
literal:
	;; Y=0 from decrunch.
	tax			; Keep number of bytes safe.
	sta	YISMAXRUN
	inc	SRCPTR		; increment src
	bne	litcop
	inc	SRCPTR+1
litcop:	lda	(SRCPTR),y
	jsr	rolling_buffer_update ; Put byte into buffer.
	eor	(DSTPTR),y	; EOR with the destination and write back into memory.
	sta	(DSTPTR),y
	iny
	cpy	#0		; Self-modified with literal-run length.
	YISMAXRUN=*-1
	bne	litcop
	jsr	incsrc
	jsr	incdst
	jmp	decrunch
backref:
	eor	#$ff		; Negate A, see below
	tax			; Keep run length safe in X.
	inx			; see above, negate is eor #$ff, then +1
	stx	sm_runlen	; Put run length into self-modifying in code.
	iny
	lda	(SRCPTR),y	; How far to go back?
	sta	@sbc
	lda	rolling_buffer_update::PTR
	SEC
	sbc	#0
	@sbc=*-1
	sta	BAKCOPPTR
	;; Copy upward as we can have longer runs even if data is only partial. E.g. first byte is literal then set AUXPTR to DSTPTR-1 and copy for 100 or so bytes. This is essentially a run-length encoding for free.
	ldy	#0		; Clear index Y, X has number of bytes.
bckcop:	lda	Code_rollingbuffer ; Copy from here in the rolling buffer, the lowbyte is calculated and set above.
	BAKCOPPTR=*-2
	inc	BAKCOPPTR	; Increment Lo and roll over at page boundary.
	jsr	rolling_buffer_update ; Put byte into buffer.
	eor	(DSTPTR),y	; EOR with the destination and write back into memory.
	sta	(DSTPTR),y
	iny			; Now go to next byte.
	dex			; Decrement the counter.
	bne	bckcop
	lda	#0		; Placeholder for run length.
	sm_runlen = *-1
	jsr	incdstA		; Adjust destination.
	lda	#2
	jsr	incsrcA		; Skip two bytes
	jmp	decrunch
incsrc:				; increment by X
	txa
incsrcA:
	clc
	adc	SRCPTR
	sta	SRCPTR
	lda	SRCPTR+1
	adc	#0
	sta	SRCPTR+1
	rts
incdst:				; increment by X
	txa
incdstA:
	clc
	adc	DSTPTR
	sta	DSTPTR
	lda	DSTPTR+1
	adc	#0
	sta	DSTPTR+1
	rts
.endproc
