;;; cl65 -t c64 -C c64-asm.cfg -u __EXEHDR__ test_georam.s
;;; https://www.c64-wiki.com/wiki/GeoRAM
;;; x64 -georam -autostart test_georam

	GEORAM := $de00
	GEOPAGE := $DFFE
	GEOBLOCK := $DFFF

	.code
	jmp	_main

fillpage:
	ldx	#0
@l:	sta	GEORAM,x
	dex
	bne	@l
	rts

	.code
_main:	lda	#$80
	jsr	fillpage
	lda	#1
	sta	GEOPAGE
	lda	#$81
	jsr	fillpage
	lda	GEORAM
	sta	$0400
	lda	#0
	sta	GEOPAGE
	lda	GEORAM
	sta	$0401
	rts
