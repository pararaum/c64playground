
	;; Import the SID song begin and end (pointer after last byte).
	.import	SIDSONG_BEGIN
	.import	SIDSONG_END
	.import	SONGNAME
	.import	SONGAUTHOR
	.import	SONGRELEASED
	.import	SONGLOADADDRPTR
	.importzp	ptr1,ptr2
	.import	setup
	.import	setup_irq
	.import _output_string20
	.import output_stringat20
	.import	copy_song

	.export	_start

	.segment	"ONCE"

	.macro	OutputMacro txtptr,col,row
	lda	txtptr
	pha
	lda	txtptr+1
	pha
	lda	#row
	pha
	lda	#col
	pha
	jsr	output_stringat20
	.endmacro

_start:
	jsr	setup
	OutputMacro	SONGNAME,12,8
	OutputMacro	SONGAUTHOR,12,10
	OutputMacro	SONGRELEASED,12,12
	lda	#<$1003
	ldx	#>$1003
	jsr	setup_irq
	jmp	main


	;; 💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾💾
	.code
main:
	lda	#<SONGLOADADDRPTR
	sta	ptr1
	lda	#>SONGLOADADDRPTR
	sta	ptr1+1
	lda	#<(SIDSONG_END-SIDSONG_BEGIN)
	ldx	#>(SIDSONG_END-SIDSONG_BEGIN)
	jsr	copy_song
	lda	#0
	jsr	$1000
	cli
	jmp	*
