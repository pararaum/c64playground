;;; Animate characters (2X2) and move them horizontally.

	.include	"kernal.i"
	.include	"irqmoiety.i"
	.import	muzak_init

	.export	_main

_main:	jsr	setup_vic
	jsr	setup_irq
	lda	#0
	jsr	muzak_init
	cli
@wait:	jsr	GETIN
	beq	@wait
	jmp	RESTOR

setup_vic:
	lda	#0
	sta	$d020
	sta	$d021
	rts

