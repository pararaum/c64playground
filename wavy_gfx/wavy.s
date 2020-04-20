
	.export	_main
	.import fillcolram
	.import	animationfont
	.import	animationfont_end
	SCREENPTR = $c000
	CHARGENPTR = $c800

	.code
_main:
	lda	#5
	sta	$d020
	sta	$d021
	lda	#3
	jsr	fillcolram
	jsr	initialise_screenptr_n_chargen
	jsr	copy_font
	rts

copy_font:
	lda	animationfont
	ldx	#0
@l:	lda	animationfont,x
	sta	CHARGENPTR,x
	inx
	cpx	#<(animationfont_end-animationfont)
	bne	@l
	rts

	.include "screen_n_char.inc"
