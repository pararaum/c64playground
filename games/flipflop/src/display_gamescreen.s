	.include	"t7d/stringfunctions.i"
	.include	"LAMAlib-macros16.inc"

	.export	_display_gamescreen

	.rodata
gamescreen:
	.incbin	"assets/gamescreen.seq"
	.byte	0

_display_gamescreen:
	ldax	#gamescreen
	jsr	output_long_string
	rts
