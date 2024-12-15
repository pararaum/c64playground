;;; https://csdb.dk/event/?id=3370
	;; File name of the Koala picture.
	.define GFXFNAME "cute_cat_ninja.koa"
	;; If defined will chose a different border colour than the background colour.
	;BORDER = 13
	;; Incremented at the beginning of the ISR and decremented at the end.
	IRQINCDEC = $2

	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"
	.import __bitmap_LOAD__
	.import __videomatrix_LOAD__

	.segment "bitmap"
	.incbin	GFXFNAME,2,8000

	.segment "videomatrix"
	.incbin	GFXFNAME,8002,1000

	.segment "colorram"
colourram:
	.incbin	GFXFNAME,9002,1000
	
	.rodata
background:
	.incbin	GFXFNAME,10002,1

	.code
	jsr	_disable_cia_irq
	lda	background
	sta	$d021
	.ifdef	BORDER
	lda	#BORDER
	.endif
	sta	$d020
	jsr	initialise_bitmap_and_screenptr
	lda	$d011		; Enable bitmap mode.
	ora	#$20
	sta	$d011
	lda	$d016		; Multicolour.
	ora	#$10
	sta	$d016
	ldx	#0
@cl1:	lda	colourram,x
	sta	$d800,x
	lda	colourram+$100,x
	sta	$d800+$100,x
	lda	colourram+$200,x
	sta	$d800+$200,x
	lda	colourram+$300,x
	sta	$d800+$300,x
	inx
	bne	@cl1
	sei
	SetIRQ314Pointer	irqfun
	EnableIRQatRasterline	49
	cli
	jmp	*

irqfun:	inc	IRQINCDEC
	;; 	jsr	muzakplay
	asl	$d019
	dec	IRQINCDEC
	jmp	$EA81

	SCREENPTR = __videomatrix_LOAD__
	BITMAPPTR = __bitmap_LOAD__
	.include	"t7d/vic/bitmap_n_screen.inc"
