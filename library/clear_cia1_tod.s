	.include	"t7d/cia/tod.i"

.proc	clear_cia1_tod
	;; Writing to TOD set clock.
	lda	$dc0f
	and	#$7f
	sta	$dc0f
	;; 50 Hz.
	lda	$dc0e
	and	#$7f
	sta	$dc0e
	lda	#0
	sta	$dc0b
	sta	$dc0a
	sta	$dc09
	sta	$dc08
	rts
.endproc
