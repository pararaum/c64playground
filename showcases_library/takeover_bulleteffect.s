	.include	"libt7d.i"
	.include	"kernal.i"
	.include	"takeover_effects/bullets_to_right_bottom.i"

	.import	__FONT0_START__
	.import	__SCREEN0_START__
	
CHARGEN_DESTINATION := __FONT0_START__

	.export	CHARGEN_DESTINATION

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
	lda	frames_before_effect
	beq	@run
	dec	frames_before_effect
	jmp	@skip
@run:
	jsr	update_takeover_bulleteffect
	bcs	@skip
	lda	#0
	sta	$d020
	sta	$d011
@skip:
	asl	$d019
	jmp	EXITIRQ

	.code
main:	sei
	jsr	init_takeover_bulleteffect
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
	jmp	*
	brk
