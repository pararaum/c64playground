
	.export	_fill_colour_ram

_fill_colour_ram:
	ldx	#0
@l:	sta	$d800,x
	sta	$d900,x
	sta	$da00,x
	sta	$db00,x
	dex
	bne	@l
	rts
