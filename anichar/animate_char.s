;;; Animate chararcter core routine.

	.export	animate_char_fontupdate
	.import	animate_char_chargenaddr

	;; Does not work as expected.
	.DEFINE	charaddr(code)	(animate_char_chargenaddr+8*code)

	.macro	Copy8Bytes	src,dst
	.local	loop
	ldx	#7
loop:	lda	src,x
	ora	lazerlight_value
	sta	dst,x
	dex
	bpl	loop
	.endmacro

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
	;; ROL without Carry, see https://wiki.nesdev.com/w/index.php/User:Zzo38/6502_programming_tricks.
	asl
	adc	#0
	asl
	adc	#0
	sta	lazerlight_value
	;;
	Copy8Bytes	animate_char_chargenaddr+8*(16*14+0),animate_char_chargenaddr
	Copy8Bytes	animate_char_chargenaddr+8*(16*14+1),animate_char_chargenaddr+8*1
	Copy8Bytes	animate_char_chargenaddr+8*(16*15+0),animate_char_chargenaddr+8*4
	Copy8Bytes	animate_char_chargenaddr+8*(16*15+1),animate_char_chargenaddr+8*5
	rts
