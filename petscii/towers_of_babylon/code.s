;;; cl65 --asm-include-dir include --asm-include-dir library towers_of_babylon.s code.s

	.include	"memoryfunctions.i"
	.include	"kernal.i"
	.import		map_data
	.import		map_colour_data
	.importzp	SCREEN_BACKGROUND_COLOUR
	.importzp	SCREEN_BORDER_COLOUR
	.importzp	COLOUR_CHAR_MC1
	.importzp	COLOUR_CHAR_MC2
	
	.segment	"STARTUP"
	.segment	"INIT"
	.segment	"ONCE"
	.code

	jsr	SCINIT
	lda	#$18		; Multicolour mode, 40 columns
	sta	$d016
	memcpy_down_macro	map_data,$0400,1000
	memcpy_down_macro	map_colour_data,$d800,1000
	lda	#SCREEN_BACKGROUND_COLOUR
	sta	$d021
	lda	#SCREEN_BORDER_COLOUR
	sta	$d020
	lda	#COLOUR_CHAR_MC1
	sta	$d022
	lda	#COLOUR_CHAR_MC2
	sta	$d023
	jmp	*		; Endless loop.
	
