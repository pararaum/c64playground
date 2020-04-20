
	.export	_main
	.import fillcolram

	.code
_main:
	lda	#5
	sta	$d020
	sta	$d021
	lda	#3
	jsr	fillcolram
	rts
