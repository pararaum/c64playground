
	.segment "LOADADDR"
	.export __LOADADDR__
__LOADADDR__:	.word $c000

	.zeropage
zpdat:	.word	0

	.data
val:	.dword	$deadbeef
	
	.code
	ldy	#$fe
	sta	zpdat
	dey
	sta	zpdat+1
	ldx	#00
l14:	inc	$d020
	dec	$d021
	inc	$d021
	dex
	bne	l14
	lda	#<l14
	sta	$0400
	lda	#>l14
	sta	$0401
	ldy	#3
l19:	lda	val,y
	sta	$0500,y
	dey
	bpl	l19
	rts
