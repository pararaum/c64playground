	.include	"t7d/libt7d.i"
	.include	"t7d/compression/xipz-qadz.decrunch.inc"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"
	.macpack	longbranch

	.import	SFXBASE,SFXPLAY,__SCREEN_START__

	.export	init_the_jump

	.bss
nextirq:	.res	2	; This will be written to the $314 irq vector when we are done.
animation:	.res	2002+1	; For decompression of the animation frame.

	.rodata
f0000:	.incbin	"temporary.frame.ski_jump.0000.xipz"
f0001:	.incbin	"temporary.frame.ski_jump.0001.xipz"
f0002:	.incbin	"temporary.frame.ski_jump.0002.xipz"
f0003:	.incbin	"temporary.frame.ski_jump.0003.xipz"
f0004:	.incbin	"temporary.frame.ski_jump.0004.xipz"
f0005:	.incbin	"temporary.frame.ski_jump.0005.xipz"
f0006:	.incbin	"temporary.frame.ski_jump.0006.xipz"
f0007:	.incbin	"temporary.frame.ski_jump.0007.xipz"
f0008:	.incbin	"temporary.frame.ski_jump.0008.xipz"
f0009:	.incbin	"temporary.frame.ski_jump.0009.xipz"
	
	.data
framecounter:	.byte	$FF	; Count the frames for animation effect.
	
	.code
init_the_jump:
	ldax	$314
	stax	nextirq
	SetIRQ314Pointer	irq
	rts

.proc waitframe
	lda	framecounter
wait:	cmp	framecounter
	beq	wait
	rts
.endproc

.proc	nextframe
	stax	ptr1
	ldax	#animation
	stax	ptr2
	jsr	decrunch_qadz
	lda	animation
	sta	$d020
	lda	animation+1
	sta	$d021
	lda	#19
wait:	cmp	framecounter
	bcs	wait
	ldx	#0
floop:	jsr	waitframe
	.repeat	25,ROW
	lda	#$5c
	sta	__SCREEN_START__+ROW*40,x
	lda	#1
	sta	$d800+ROW*40,x
	.endrepeat
	jsr	waitframe
	.repeat	25,ROW
	lda	2+animation+ROW*40,x
	sta	__SCREEN_START__+ROW*40,x
	lda	2+1000+animation+ROW*40,x
	sta	$d800+ROW*40,x
	.endrepeat
	inx
	cpx	#40
	jne	floop
	ldx	#0
	stx	framecounter	; Reset the framecounter
	rts
.endproc

.proc	irq
	asl	$d019		; Acknowledge the interrupt.
	inc	framecounter	; Increment our frame counter.
	beq	firsttime	; Can only be zero at the first run! See variable!
	jsr	SFXPLAY
	jmp	$ea31
firsttime:			; This is our second thread where we run the animation.
	cli			; Enable further interrupts.
	ldax	#f0000
	jsr	nextframe
	ldax	#f0001
	jsr	nextframe
	ldax	#f0002
	jsr	nextframe
	ldax	#f0003
	jsr	nextframe
	ldax	#f0004
	jsr	nextframe
	ldax	#f0005
	jsr	nextframe
	ldax	#f0006
	jsr	nextframe
	ldax	#f0007
	jsr	nextframe
	ldax	#f0008
	jsr	nextframe
	ldax	#f0009
	jsr	nextframe
	sei
	lda	#1
	jsr	SFXBASE
	cli
	ldax	nextirq
	stax	$314
	cli
	jmp	$ea81
.endproc
