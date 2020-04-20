
	.export	_main
	.export SCREENPTR
	.export wavymation_chargenptr
	.import fillcolram
	.import	wavymation_copy_font

	SCREENPTR = $c000
	CHARGENPTR = $c800
	wavymation_chargenptr = CHARGENPTR

	.code
_main:
	lda	#5
	sta	$d020
	sta	$d021
	lda	#3
	jsr	fillcolram
	jsr	initialise_screenptr_n_chargen
	jsr	wavymation_copy_font
	rts


	.include "screen_n_char.inc"
