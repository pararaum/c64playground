
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

	.code
.proc main
	lda	#<SONGLOADADDRPTR
	sta	ptr1
	lda	#>SONGLOADADDRPTR
	sta	ptr1+1
	lda	#<(SIDSONG_END-SIDSONG_BEGIN)
	ldx	#>(SIDSONG_END-SIDSONG_BEGIN)
	jsr	copy_song
	lda	#0		; Initialise song.
	jsr	$FFFF
	MUZAKVECTOR = *-2
	cli
	jmp	*
.endproc

	.segment	"ONCE"
_start:
	jsr	setup
	OutputMacro	SONGNAME,12,8
	OutputMacro	SONGAUTHOR,12,10
	OutputMacro	SONGRELEASED,12,12
	;; Init address is at $a in the header.
	lda	SIDSONG_BEGIN+$a+1 ; Address is stored big endian!
	ldx	SIDSONG_BEGIN+$a
	sta	main::MUZAKVECTOR
	stx	main::MUZAKVECTOR+1
	;; Play address is at $c in the header.
	lda	SIDSONG_BEGIN+$c+1 ; Address is stored big endian!
	ldx	SIDSONG_BEGIN+$c
	jsr	setup_irq
	jmp	main
