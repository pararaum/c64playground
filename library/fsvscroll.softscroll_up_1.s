	.macpack	generic
	.include	"t7d/scroller/fsvscroll.i"

	.export	fsvscroll_softscroll_up_1

.proc	fsvscroll_softscroll_up_1
	lda	fsvscroll_softscroll_val		; Get current scroll value.
	sub	#1		; Move up.
	and	#7		; And reduce to 0..7.
	sta	fsvscroll_softscroll_val
	rts
.endproc
