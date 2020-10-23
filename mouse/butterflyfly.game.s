	.include	"libt7d.i"

	.export	game

	.data
scrollpos:	.byte	0

	.bss
flipflag:	.res	1	; Will be set to nonzero after irq

	.code
irq:
	inc	$d020
	asl	$d019		; Acknowledge IRQ
	;; Stop the scroll.
	lda	#%01111000
	sta	$d011
	
	;; Wait for the bottom.
@w:	bit	$d011
	bpl	@w
	;; Set new scroll position.
	lda	scrollpos
	and	#7
	ora	#%00010000	; Screen on, text mode, 24 rows
	sta	$d011
	lda	#$ff
	sta	flipflag
	dec	$d020
	jmp	$ea81

init_vic:
	lda	#6
	sta	$d020
	sta	$d021
	lda	#%00011000	; Text mode, raster irq bit#8 = 0
	sta	$d011
	lda	#5
	jsr	_fill_colour_ram
	rts

init_irq:
	jsr	_disable_cia_irq
	lda	#<irq
	sta	$314
	lda	#>irq
	sta	$315
	lda	#222
	sta	$d012
	lda	#%00000001
	sta	$d01a
	rts

game:
	jsr	init_vic
	jsr	init_irq
	cli
	lda	#$58
	ldx	#0
@l:	sta	$0400,x
	sta	$0500,x
	sta	$0600,x
	sta	$0700-24,x
	dex
	bne	@l
	;; 
@l1:	bit	flipflag
	bpl	@l1
	dec	scrollpos
	lda	#0
	sta	flipflag	; Clear flipflag.
	jmp	@l1
	rts

