	.text
func1:
	lda	#$5
	sta	$d020
	inc	f1zp
	rts

	.zero
f1zp:	.byte	0
