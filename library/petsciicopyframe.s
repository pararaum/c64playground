	.include	"t7d/petsciicopyframe.i"
	.include	"LAMAlib-macros16.inc"

	.importzp	ptr1

petsciicopyframe:
	stax	ptr1		; Set pointer in order to get background and border.

	addax	#2		; Skip background and border colour.
	stax	SRCPTR0
	addax	#250
	stax	SRCPTR1
	addax	#250
	stax	SRCPTR2
	addax	#250
	stax	SRCPTR3
	addax	#250
	stax	SRCPTR4
	addax	#250
	stax	SRCPTR5
	addax	#250
	stax	SRCPTR6
	addax	#250
	stax	SRCPTR7

	ldy	#0
	lda	(ptr1),y
	sta	$d020
	iny
	lda	(ptr1),y
	sta	$d021

	ldx	#0		; 250 Bytes, 0..249
loop:	;; First copy the characters.
	lda	$400,x
	SRCPTR0 = *-2
	sta	PETSCIICOPY_SCREEN,x
	lda	$400,x
	SRCPTR1 = *-2
	sta	PETSCIICOPY_SCREEN+250,x
	lda	$400,x
	SRCPTR2 = *-2
	sta	PETSCIICOPY_SCREEN+2*250,x
	lda	$400,x
	SRCPTR3 = *-2
	sta	PETSCIICOPY_SCREEN+3*250,x
	;; Second copy the colour information into colour RAM.
	lda	$400,x
	SRCPTR4 = *-2
	sta	$D800+0*250,x
	lda	$400,x
	SRCPTR5 = *-2
	sta	$D800+1*250,x
	lda	$400,x
	SRCPTR6 = *-2
	sta	$D800+2*250,x
	lda	$400,x
	SRCPTR7 = *-2
	sta	$D800+3*250,x
	;; Go to next char/colour.
	inx
	cpx	#250		; End reached?
	bne	loop
	rts
