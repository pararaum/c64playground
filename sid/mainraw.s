;;; This main program is used if the song is in `raw` format without any additional headers and only with a load address given.

	.import	SIDSONG_BEGIN
	.import	SIDSONG_END
	.import	SONGNAME
	.import	SONGAUTHOR
	.import	SONGRELEASED
	.import	copy_song
	.importzp	ptr1,ptr2
	.import	setup
	.import	setup_irq
	.import output_stringat20

	.export	_start

	.macro	OutputMacro txtptr,col,row
	lda	#<txtptr
	pha
	lda	#>txtptr
	pha
	lda	#row
	pha
	lda	#col
	pha
	jsr	output_stringat20
	.endmacro

	.segment	"ONCE"
_start:
	jsr	setup
	OutputMacro	SONGNAME,12,8
	OutputMacro	SONGAUTHOR,12,10
	OutputMacro	SONGRELEASED,12,12
	lda	#<$1003
	ldx	#>$1003
	jsr	setup_irq
	jmp	main

	.code
main:
	lda	#<SIDSONG_BEGIN
	sta	ptr1
	lda	#>SIDSONG_BEGIN
	sta	ptr1+1
	lda	#<(SIDSONG_END-SIDSONG_BEGIN)
	ldx	#>(SIDSONG_END-SIDSONG_BEGIN)
	jsr	copy_song
	lda	#0
	jsr	$1000
	cli
	jmp	*
