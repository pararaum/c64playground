	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/sprite/spritecanvas.i"

	.segment	"INIT"
	.segment	"ONCE"
	.segment	"STARTUP"
	jmp	main

SPRITECANVAS_SPRPTR=$400+1024-8
CANVASYPOS=56

irq:	asl	$d019
	lda	$d020
	pha
	inc	$d020
	jsr	spritecanvas3X4_irq
	pla
	sta	$d020
	jmp	$EA31


	.code
fill:	ldx	#0
l2:	txa
	.repeat	4,I
	sta	$3c00+I*$100,x
	.endrepeat
	dex
	bne	l2
	rts


main:	sei
	jsr	_disable_cia_irq
	jsr	busywait_frame_pm
	SetIRQ314Pointer	irq
	EnableIRQatRasterline	CANVASYPOS-3
	jsr	fill
	jsr	spritecanvas3X4_init
	.word	36		; X-position
	.byte	CANVASYPOS	; Y-position
	.byte	$f0		; sprite pointer to top-left square
	.word	SPRITECANVAS_SPRPTR ; Sprite pointer address of 0-th sprite.
	.byte	7		; Colour
	.byte	8		; Delay time
	cli
	rts
