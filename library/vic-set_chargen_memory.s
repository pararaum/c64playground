	.import	tmp1
;;; Input: A=HI of screen memory address
.proc	set_chargen_memory
	lsr
	lsr
	and	#%00001110
	sta	tmp1
	lda	$d018
	and	#%11110000
	ora	tmp1
	sta	$d018
	rts
	.endproc
