	.include	"t7d/libt7d.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/kernal.i"
	.include	"t7d/vic/vicmacros.i"
	
	.data
font:
	;; 	.incbin	"Panda.both.64c",2
	.incbin	"Flamboyant.both.64c",2
	;; We add the uppercase version as we may need the graphics characters.
	.incbin	"Flamboyant.upper.64c",2

	.code
	ldx	#0
@loop:	sei
	lda	1
	pha
	memoryconfig_ram
	.REPEAT	16,I
	lda	font+I*$100,x
	sta	$d000+I*$100,x
	.ENDREPEAT
	pla
	sta	1
	cli
	dex
	bne	@loop
	;; 	SwitchVICBank	3
	lda	#0
	sta	$dd00
	SetScreenMemory	$cc00
	SetChargenAddress $d000
	lda	#$cc		; Set high pointer of screen memory.
	sta	$288
	lda	#$1b		; Default value, text screen.
	sta	$d011
	lda	#$8
	sta	$d016		; 40 columns, no multicolour
	lda	#$93
	jsr	CHROUT
	rts
