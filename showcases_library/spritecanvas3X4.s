	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"

	.import	spritecanvas_irq
	.import spritecanvas_init

	.segment	"INIT"
	.segment	"ONCE"
	.segment	"STARTUP"
	jmp	main

SPRITECANVAS_SPRPTR=$400+1024-8

irq:	asl	$d019
	lda	$d020
	pha
	inc	$d020

	jsr	spritecanvas_irq

	
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
	EnableIRQatRasterline	50
	lda	#$ff
	sta	$d017
	sta	$d01d
	jsr	fill
	jsr	spritecanvas_init
	.word	36		; X-position
	.byte	56		; Y-position
	.byte	$f0		; sprite pointer to top-left square
	.word	SPRITECANVAS_SPRPTR ; Sprite pointer address of 0-th sprite.
	.byte	7		; Colour
	.byte	8		; Delay time
	cli
	rts
