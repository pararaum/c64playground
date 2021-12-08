
	.import	CHARGEN_DESTINATION

	.export	copy_chargen

	.code
copy_chargen:
	lda	1
	pha
	lda	#$33		; Map in chargen.
	sta	1
	ldx	#0
@l1:
	.repeat 8,I
	lda	$d000+$100*I,x
	sta	CHARGEN_DESTINATION+$100*I,x
	.endrepeat
	dex
	bne	@l1
	pla
	sta	1
	rts
