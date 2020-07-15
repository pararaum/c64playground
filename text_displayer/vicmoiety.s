	.export vic_init
	.import main
	.import charsetdata
	.import charsetaddr

vic_init:
	lda	#7		; Yellow
	sta	$d020
	sta	$d021
	lda	#$9c		; Purple [http://sta.c64.org/cbm64pet.html]
	jsr	$ffd2
	lda	#$93		; Clear
	jsr	$ffd2
	;; Screen at $400, charset at $800
	lda	#(($0400/$0400)<<4)|<(charsetaddr/$0400)
	sta	$d018
	rts
