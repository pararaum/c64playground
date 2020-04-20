	.export fillcolram


;;; Fills the colour ram at $D800 with the value in the accumulator.
;;; Input: A=colour
;;; Modifies: A/X
	.code
fillcolram:
	ldx	#0
@l:	sta	$D800,x
	sta	$D900,x
	sta	$DA00,x
	sta	$DB00,x
	inx
	bne	@l
	rts
