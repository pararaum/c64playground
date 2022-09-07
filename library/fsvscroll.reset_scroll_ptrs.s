	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/scroller/fsvscroll.i"

.proc	fsvscroll_reset_scroll_ptrs
	P_transfer	fsvscroll_character_addr,fsvscroll_character_ptr
	P_transfer	fsvscroll_colour_addr,fsvscroll_colour_ptr
	rts
.endproc
