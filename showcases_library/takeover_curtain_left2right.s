	.include	"libt7d.i"
	.include	"t7d/kernal.i"
	.include	"takeover_effects/curtain_left2right.i"

	.import	__SCREEN0_START__
	TAKEOVER_SCREENBASE=__SCREEN0_START__

	.export TAKEOVER_SCREENBASE

	.export	ptr1
	.export tmp1

TAKEOVER_SPRITEBUFFER=$200

	.zeropage
ptr1:	.res	2
tmp1:	.res	1

	.data
frames_before_effect:		; Number of frames before the effect starts.
	.byte	121

	.segment	"LOADADDR"
	.word	$0801

	.segment	"EXEHDR"
	.word	next_line
	.word	7		; Line number
	.byte	$9e		; SYS
	.byte	"(2071) : ",$8f," T7D"
next_line:	.byte	0,0,0
	jmp	once

	.segment	"ONCE"
once:	lda	#0
	jsr	$1000
	jmp	main

	.segment	"MUZAK"
	.incbin	"../sid/bananas-01.sid",$7c+2

irq:	jsr	$1003
	;; 	inc	$d020
	jsr	update_takeover_curtain_left2right
	;; 	dec	$d020
	asl	$d019
	jmp	EXITIRQ


	.code
main:	sei
	lda	#121
	jsr	init_takeover_curtain_left2right
	jsr	_disable_cia_irq
	SetIRQ314Pointer	irq
	lda	#0		; Set rasterline
	sta	$d012
	lda	$d011
	and	#$7f
	sta	$d011
	lda	#1		; Enable IRQ
	sta	$d01a
	cli
l1:	lda	takeover_curtain_left2righ
	bne	l1
	lda	#13
	sta	$d020
	lda	#0
	sta	$d011
	jmp	*
	brk
