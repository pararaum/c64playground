	.include	"t7d/memoryfunctions.i"
	.include	"LAMAlib-macros16.inc"

	.import	_waitframes

	.export	_titlescreen

	.data
petsciiscreen:
	.incbin	"frame.petscii.data"

	.code
_titlescreen:
	lda	$d020
	pha
	lda	$d021
	pha
	lda	petsciiscreen
	sta	$d020
	lda	petsciiscreen+1
	sta	$d021
	Memcpy1KInline	petsciiscreen+2, $0400
	Memcpy1KInline	petsciiscreen+2+1000, $d800
	ldax	#200
	jsr	_waitframes
	tay			; Save A.
	pla
	sta	$d021
	pla
	sta	$d020
	tya			; Restore A.
	rts

