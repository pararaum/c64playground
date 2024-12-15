	.include	"t7d/memoryconfig.i"
	.include	"t7d/theatre.i"

	.export	_theatre_copy_text2spare
	.export	_theatre_copy_spare2text

.proc	_theatre_copy_text2spare
	lda	1
	pha
	memoryconfig_io
	ldx	#0
loop:
	.repeat	4,I
	 lda	::THEATRE_TEXT+I*($100-8),x
	 sta	::THEATRE_SPARE_TEXT+I*($100-8),x
	 lda	$d800+I*($100-8),x
	 sta	::THEATRE_SPARE_COLS+I*($100-8),x
	.endrepeat
	inx
	bne	loop
	pla
	sta	1
	rts
.endproc

.proc	_theatre_copy_spare2text
	lda	1
	pha
	memoryconfig_io
	ldx	#0
loop:
	.repeat	4,I
	 lda	::THEATRE_SPARE_TEXT+I*($100-8),x
	 sta	::THEATRE_TEXT+I*($100-8),x
	 lda	::THEATRE_SPARE_COLS+I*($100-8),x
	 sta	$d800+I*($100-8),x
	.endrepeat
	inx
	bne	loop
	pla
	sta	1
	rts
.endproc
