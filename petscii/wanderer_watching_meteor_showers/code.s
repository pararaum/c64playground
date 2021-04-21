;;; cl65 --asm-include-dir include --asm-include-dir library wanderer\'s_portents.s code.s

	.include	"memoryfunctions.i"
	.include	"kernal.i"
	.import		map_data
	.import		map_colour_data
	.importzp	SCREEN_BACKGROUND_COLOUR
	.importzp	SCREEN_BORDER_COLOUR
	
	.segment	"STARTUP"
	.segment	"INIT"
	.segment	"ONCE"
	.code

	jsr	SCINIT
	memcpy_down_macro	map_data,$0400,1000
	memcpy_down_macro	map_colour_data,$d800,1000
	lda	#SCREEN_BACKGROUND_COLOUR
	sta	$d021
	lda	#SCREEN_BORDER_COLOUR
	sta	$d020
	jmp	*		; Endless loop.
	
