
	.importzp	ptr1, ptr2

	.export	memcpy_up

;;; ptr1 to ptr2, A/X=size
.proc	memcpy_up
	sta	sizeLO		; LO of size saved for later use (self-modifying)
	; the last byte must be moved first (X contains sizeHI)
	clc			; start at the final pages of PTR1 and PTR2
	txa
	adc	ptr1+1
	sta	ptr1+1
	clc
	txa
	adc	ptr2+1
	sta	ptr2+1
	inx	     ; allows the use of BNE after the DEX below
	ldy	#0
sizeLO=*-1
	beq	mup3
	dey	     ; move bytes on the last page first
	beq	mup2
mup1:
	lda	(ptr1),y
	sta	(ptr2),y
	dey
	bne	mup1
mup2:
	lda	(ptr1),y	; handle Y = 0 separately
	sta	(ptr2),y
mup3:
	dey
	dec	ptr1+1	; move the next page (if any)
	dec	ptr2+1
	dex
	bne	mup1
	rts
.endproc
