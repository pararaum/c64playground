;;; Animate chararcter core routine.

	.export	animate_char_fontupdate
	.import	animate_char_chargenaddr

	.data
;;; The laserlight is the vertical line scrolling on the whole screen from left to right. It will be put into the character $ff
lazerlight_value:
	.byte	%10000000

	.code

animate_char_fontupdate:
	lda	lazerlight_value
	sta	animate_char_chargenaddr+$ff*8
	sta	animate_char_chargenaddr+$ff*8+1
	sta	animate_char_chargenaddr+$ff*8+2
	sta	animate_char_chargenaddr+$ff*8+3
	sta	animate_char_chargenaddr+$ff*8+4
	sta	animate_char_chargenaddr+$ff*8+5
	sta	animate_char_chargenaddr+$ff*8+6
	sta	animate_char_chargenaddr+$ff*8+7
	asl
	adc	#0
	asl
	adc	#0
	sta	lazerlight_value
	rts
