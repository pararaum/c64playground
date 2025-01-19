	.include	"t7d/petsciicopy.i"
	.include	"LAMAlib-macros16.inc"

;;; TODO: check and compare with theatre.copy_petscii_frame.s and theatre.theatre_copy_compressed_frame.s.

	.import	ptr1

petsciicopyframe:
	stax	ptr1		; Set pointer in order to get background and border.

	stax	SRCPTR0
	inx
	stax	SRCPTR1
	

	ldy	#0
	lda	(ptr1),y
	sta	$d020
	iny
	lda	(ptr1),y
	sta	$d021

	

	lda	$400,x
	SRCPTR0 = *-2
	sta	PETSCIICOPY_SCREEN,x
	lda	$400,x
	SRCPTR1 = *-2
	sta	PETSCIICOPY_SCREEN+$100,x
	lda	$400,x
	SRCPTR2 = *-2
	sta	PETSCIICOPY_SCREEN+$200,x
	lda	$400,x
	SRCPTR3 = *-2
	sta	PETSCIICOPY_SCREEN+$2e8,x ; (+ #x400 1000)

	rts
