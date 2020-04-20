
	.export	animationfont
	.export animationfont_end
	.export	wavymation_copy_font
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
	
