
	.export _clear_logical_line

_clear_logical_line:
	lda	#' '
	ldy	#39
l1:	sta	($d1),y
	dey
	bpl	l1
	rts
