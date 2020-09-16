	.include	"libt7d.i"
	.include	"animate_char.i"
	.import	muzak_play

	RASTERLINE = 2

	.export	setup_irq

	.bss
oldirq:	.res	2

	.code

irqroutine:
	inc	$d020
	jsr	muzak_play
	inc	$d020
	jsr	animate_char_fontupdate
	lda	#0
	sta	$d020
	asl	$d019
	jmp	(oldirq)

setup_irq:
	jsr	_disable_cia_irq
	lda	$314
	sta	oldirq
	lda	#<irqroutine
	sta	$314
	lda	$315
	sta	oldirq+1
	lda	#>irqroutine
	sta	$315
	lda	$d011		; Set raster irq line bit #8.
	and	#%01111111
	sta	$d011
	lda	#RASTERLINE	; Rasterline
	sta	$d012
	lda	#%00000001	; Enable raster interrupt
	sta	$d01a
	rts
