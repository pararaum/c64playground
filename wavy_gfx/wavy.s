
	.export	_main
	.export SCREENPTR
	.export wavymation_chargenptr
	.export wavymation_screenptr
	.import fillcolram
	.import	wavymation_copy_font
	.import wavymation_animate_font
	.import wavymation_generate_image

	SCREENPTR = $c000
	CHARGENPTR = $c800
	wavymation_chargenptr = CHARGENPTR
	wavymation_screenptr = SCREENPTR

	.code
	.include "screen_n_char.inc"

	.rodata
logo:
	.incbin	"logo.t7d.40x25.pbm",9
sieve:
	.incbin	"image.40x25.sieve.pbm",9

	.data
framecounter:	.byte	0

	.macro	wavycall	img
	.scope
	lda	#509/3
	sta	framecounter
	lda	#<img
	ldx	#>img
	jsr	wavymation_generate_image
loop:
	jsr	waitframe
	jsr	waitframe
	jsr	waitframe
	jsr	wavymation_animate_font
	dec	framecounter
	bne	loop
	.endscope
	.endmacro
	
	.code
_main:
	lda	#5
	sta	$d020
	sta	$d021
	lda	#3
	jsr	fillcolram
	jsr	initialise_screenptr_n_chargen
	jsr	wavymation_copy_font
	jsr	fill
	lda	#<logo
	ldx	#>logo
	jsr	wavymation_generate_image
	sei
	wavycall	logo
	wavycall	sieve
	brk


waitframe:
@l1:	bit	$d011
	bmi	@l1
@l2:	bit	$d011
	bpl	@l2
	rts

fill:
	ldy	#25-1
	lda	#0
@l2:	ldx	#0
@l1:
	pha			; Store current value
@ml:	sec
	sbc	#12
	bcs	@ml		; Modulus loop.
	adc	#12
	clc
	adc	#1
	sta	SCREENPTR,x
	@fillptr = *-2
	sbc	#0		; C is always cleared by the above operations.
	pla
	clc
	adc	#1
	inx
	cpx	#40
	bne	@l1
	sec
	sbc	#39
	pha
	lda	@fillptr
	clc
	adc	#40
	bcc	@s
	inc	@fillptr+1
@s:
	sta	@fillptr
	pla
	dey
	bne	@l2
	rts
