	.import	__MUZAK_RUN__

	.export	mainirq
	.export	_framecounter
	.export	_get_framecounter

	.data
_framecounter:	.byte	$ff, $ff

	.code
mainirq:
	asl	$d019
	jsr	__MUZAK_RUN__+3
	inc	_framecounter
	bne	nocarr
	inc	_framecounter+1
nocarr:
	jmp	$EA34		; No run/stop!

.proc	_get_framecounter
	sei
	lda	_framecounter
	ldx	_framecounter+1
	cli
	rts
.endproc
