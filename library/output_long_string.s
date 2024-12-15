	.include	"t7d/kernal.i"

	.export	output_long_string

	.code
.proc	output_long_string
	sta	strptr
	stx	strptr+1
loop:	lda	$FFFF
	strptr=*-2
	beq	out
	jsr	CHROUT
	inc	strptr
	bne	loop
	inc	strptr+1
	bne	loop
out:	rts
.endproc
