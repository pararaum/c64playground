
	.export	animationfont
	.export animationfont_end
	.export	wavymation_copy_font
	.export wavymation_animate_font
	.import wavymation_chargenptr

	.code
wavymation_copy_font:
	lda	animationfont
	ldx	#0
@l:	lda	animationfont,x
	sta	wavymation_chargenptr,x
	inx
	cpx	#<(animationfont_end-animationfont)
	bne	@l
	rts

wavymation_animate_font:
	ldx	#0
@l1:	lda	wavymation_chargenptr+8,x ; Save first character.
	pha
	inx
	cpx	#8
	bne	@l1
	ldx	#0
@l2:	lda	wavymation_chargenptr+16,x
	sta	wavymation_chargenptr+8,x
	inx
	cpx	#8*(12-1)
	bne	@l2
	ldx	#8-1
@l3:	pla
	sta	wavymation_chargenptr+8+11*8,x
	dex
	bpl	@l3
	rts

	.data
animationfont:
	.res	8,0
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000011
	.byte	%00000011
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000110
	.byte	%00000110
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00001100
	.byte	%00001100
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00011000
	.byte	%00011000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00110000
	.byte	%00110000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%01100000
	.byte	%01100000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%11000000
	.byte	%11000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%01100000
	.byte	%01100000
	.byte	%00000000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00110000
	.byte	%00110000
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00011000
	.byte	%00011000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00001100
	.byte	%00001100
	.byte	%00000000
	;;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000110
	.byte	%00000110
	.byte	%00000000
	.byte	%00000000
animationfont_end:
	
