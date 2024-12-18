
	.include	"t7d/kernal.i"

	.data
mwcommand:
	.byte	"m-w"
mwcommand_addr:
	.word	$500
mwcommand_len:
	.byte	0
mwcommand_buff:
	.res	32
mwcommand_end:

	.segment	"EXEHDR"
	.word	next
	.word	main
	.byte	$9e," ",$30+<((main/1000) .mod 10),$30+<((main/100) .mod 10),$30+<((main/10) .mod 10),$30+<(main .mod 10)
next:	.res	3
	.word	1

	.code
main:	nop
	;;  Use $BA?
	lda	#8
	jsr	LISTEN		; Send listen to drive 8.
	lda	#$60+$f
	jsr	LSTNSA		; Listen secondary address, $60 is listen, $f is channel.
	ldx	#0
l1:	lda	mwcommand,x
	jsr	IECOUT
	inx
	cpx	#mwcommand_end-mwcommand
	bne	l1
	jsr	UNLSTN		; Drive 8 can now stop listening.
	rts
