
	* = $8000

	.word	coldstart
	.word	warmstart
	.byte	$c3,$c2,$cd,$38,$30

warmstart:
	inc	$d020
	rti


coldstart:	.(
	jsr	$fda3
	jsr	$fd50
	jsr	$fd15
	jsr	$ff5b
	lda	#1
	sta	$d020
	sta	$d021
l1:	inc	$0400
	jmp	l1
	.)
