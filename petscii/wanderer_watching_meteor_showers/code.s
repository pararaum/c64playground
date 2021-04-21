;;; cl65 --asm-include-dir include --asm-include-dir library wanderer\'s_portents.s code.s

	.include	"memoryfunctions.i"
	.include	"kernal.i"
	.import		map_data
	.import		map_colour_data
	
	.segment	"STARTUP"
	.segment	"INIT"
	.segment	"ONCE"
	.code

	jsr	SCINIT
	memcpy_down_macro	map_data,$0400,1000
	memcpy_down_macro	map_colour_data,$d800,1000
	rts
	
