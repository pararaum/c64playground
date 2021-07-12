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

	.code
.proc main
	lda	#<SIDSONG_BEGIN
	sta	ptr1
	lda	#>SIDSONG_BEGIN
	sta	ptr1+1
	lda	#<(SIDSONG_END-SIDSONG_BEGIN)
	ldx	#>(SIDSONG_END-SIDSONG_BEGIN)
	jsr	copy_song
	lda	#0
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
	lda	#<SIDSONG_BEGIN	; Set ptr1 to the load address of the song.
	sta	ptr1
	lda	#>SIDSONG_BEGIN
	sta	ptr1+1
	;; Assumption is loadaddress+0=Init, loadaddress+3=play
	ldy	#0
	lda	(ptr1),y	; init LO
	sta	main::MUZAKVECTOR
	iny
	lda	(ptr1),y	; init HI
	sta	main::MUZAKVECTOR+1
	lda	main::MUZAKVECTOR ; init LO
	clc
	adc	#3		; loadadress+3=play
	pha			; Store temporarily on stack.
	lda	main::MUZAKVECTOR+1 ; init HI
	adc	#0		    ; Add carry, if any
	tax			    ; Put play HI into X
	pla			    ; Play LO in A
	jsr	setup_irq
	jmp	main

	
