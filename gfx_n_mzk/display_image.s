;;; Simple image viewer.
;;; cl65 --asm-include-dir ../includeCC65/ -C display_image.cfg display_image.s

	.import __bitmap_LOAD__
	.import __videomatrix_LOAD__

	.segment "bitmap"
	.incbin	"megapixel-pararaum.koa",2,8000

	.segment "videomatrix"
	.incbin	"megapixel-pararaum.koa",8002,1000

	.segment "colorram"
colourram:
	.incbin	"megapixel-pararaum.koa",9002,1000
	
	.rodata
background:
	.incbin	"megapixel-pararaum.koa",10002,1

	.code
	lda	background
	sta	$d020
	sta	$d021
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
	jmp	*

	SCREENPTR = __videomatrix_LOAD__
	BITMAPPTR = __bitmap_LOAD__
	.include	"bitmap_n_screen.inc"

