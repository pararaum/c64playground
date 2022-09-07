	.include	"t7d/scroller/fsvscroll.i"

.proc	_fsvscroll_default_draw_next_up_line_and_rotate
	jsr	fsvscroll_draw_next_line_at_bottom
	bne	@s1
	jsr	fsvscroll_reset_scroll_ptrs
@s1:
	rts
.endproc
