	.importzp	tmp1
	.export	set_screen_memory

;;; Input: A=HI of screen memory address
	.proc	set_screen_memory
	asl
	asl
	sta	tmp1
	lda	$d018
	and	#%00001111
	ora	tmp1
	sta	$d018
	rts
	.endproc
