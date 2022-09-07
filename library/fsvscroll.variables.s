	.include	"t7d/scroller/fsvscroll.i"
	
	.zeropage
fsvscroll_character_ptr:
	.res	2
fsvscroll_colour_ptr:
	.res	2

	.bss
fsvscroll_character_addr:
	.res	2
fsvscroll_colour_addr:
	.res	2
fsvscroll_character_end:
	.res	2

	.data
fsvscroll_copy_next_line_fun:
	.word	_fsvscroll_default_draw_next_up_line_and_rotate

	.bss
fsvscroll_softscroll_val:
	.res	1
fsvscroll_d018_pageflip_eor:
	.res	1
