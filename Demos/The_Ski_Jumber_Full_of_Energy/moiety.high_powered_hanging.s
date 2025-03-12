	.include	"LAMAlib.inc"
	.include	"t7d/libt7d.i"
	.include	"t7d/petsciicopyframe.i"

	.import	_frame0001,_frame0002,_frame0003
	.import	SFXBASE,SFXPLAY

	.export	init_high_powered_hanging

DEFAULTWAIT=7*50+1

	.bss
framecounter:	.res	2

	.data
nextirq:	.res	2	; Next interrupt pointer
currentsubroutine:	.byte	0 ; Which of the irq subroutines to use?

	.code
irq:	asl	$d019
	jsr	SFXPLAY
	lda	currentsubroutine
 	on_A_jsr0	irqsub_wait,irqsub_copyhanging,irqsub_wait,bzzt,irqsub_copyelectricity,irqsub_setframecounterLO,irqsub_flash,irqsub_wait,irqsub_copyashes,irqsub_finish
	;lda	$d011
	;pha
	;and	#<~(1<<3|$80)
	;sta	$d011
	;pla
	;sta	$d011
	jmp	$ea31

.proc	bzzt
	lda	#2
	jsr	SFXBASE
	inc	currentsubroutine
	rts
.endproc


.proc	irqsub_wait
	lda	framecounter
	ora	framecounter+1
	bne	out
	ldax	#DEFAULTWAIT
	stax	framecounter
	inc	currentsubroutine
out:	dec16	framecounter
	rts
.endproc

.proc	irqsub_copyhanging
	inc	currentsubroutine
	lda	#3
	jsr	SFXBASE
	lda	#96
	sta	framecounter
	lda	#0
	sta	framecounter+1
	cli			; Enable further interrupts as copying may take time.
	ldax	#_frame0001
	jmp	petsciicopyframe
.endproc


.proc	irqsub_copyelectricity
	inc	currentsubroutine
	cli			; Enable further interrupts as copying may take time.
	ldax	#_frame0003
	jmp	petsciicopyframe
.endproc


.proc	irqsub_copyashes
	inc	currentsubroutine
	cli			; Enable further interrupts as copying may take time.
	ldax	#_frame0002
	jmp	petsciicopyframe
.endproc


.proc	irqsub_setframecounterLO
	lda	#197
	sta	framecounter
	lda	#0
	sta	framecounter+1
	inc	currentsubroutine
	rts
.endproc


.proc	irqsub_flash
	dec	framecounter
	beq	out
	ldx	#39		; 40-1 columns to change.
	lda	framecounter	; LO of framecounter
	and	#1		; Lowest bit?
	beq	yellow		; else black
	lda	#7
	sta	$d021
	lda	#0
	jmp	flash
yellow:
	lda	#0
	sta	$d021
	lda	#7
flash:
	.repeat	25,I
	sta	$d800+40*I,x
	.endrepeat
	dex
	bpl	flash
	rts
	;; Leave this interrupt routine.
out:	ldax	#30
	stax	framecounter
	inc	currentsubroutine
	rts
.endproc


.proc	irqsub_finish
	ldax	nextirq
	stax	$314
	rts
.endproc

init_high_powered_hanging:
	ldax	$314
	stax	nextirq
	SetIRQ314Pointer	irq
	ldax	#40
	stax	framecounter
	rts

	
