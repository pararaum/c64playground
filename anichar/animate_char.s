;;; Animate chararcter core routine.

;;; Font includes the animation (organised as 16×16) in the last two rows. This animation will be copied in the first eight characters. The animation is considered to by 2×2!
	
;  +---------------+
;  |@ABCDEFG       |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |               |
;  |@A             |
;  |DE             |
;  +---------------+

; Animation Display is
; @ABC
; DEFG
	.include	"pseudo16.inc"
	.export	animate_char_fontupdate
	.import	animate_char_chargenaddr
	.macpack generic

	EMPTY_CHAR_CELL = $FF

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


	.zeropage
;;; This is a three byte buffer in the zeropage where a single line of the animation. It is in the zeropage for speed (ROR).
b3buffer:	.res	3
;;; Pointers for all character copy operations.
	;; Source pointer
acsrc:	.res	2
	;; Destination pointer
acdst:	.res	2

	.rodata
;;; Pointers to the animation character. Four animations of 2×2 characters.
animation_pointers:
	.word	animate_char_chargenaddr+8*(16*14+0)
	.word	animate_char_chargenaddr+8*(16*14+2)
	.word	animate_char_chargenaddr+8*(16*14+4)
	.word	animate_char_chargenaddr+8*(16*14+6)

	.data
;;; The lazerlight is the vertical line scrolling on the whole screen from left to right. It will be put into the character EMPTY_CHAR_CELL
lazerlight_value:
	.byte	%10000000

	.code

;;; Update the "empty" value, this is used to fill the screen where no animation is. It will also update the lazerlight value.
;;; Input: lazerlight_value
;;; Output: lazerlight_value
;;; Modifies: A
update_empty_value:
	lda	lazerlight_value
	;; ROL without Carry, see https://wiki.nesdev.com/w/index.php/User:Zzo38/6502_programming_tricks.
	asl
	adc	#0
	asl
	adc	#0
	sta	lazerlight_value
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+1
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+2
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+3
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+4
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+5
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+6
	sta	animate_char_chargenaddr+EMPTY_CHAR_CELL*8+7
	rts

;;; Copy a single char line (two bytes) into the 3buffer, clearing the last byte in the buffer.
;;; Input: Y=offset in current char for zigzag motion, acsrc, acdst
;;; Output: Y=Y+8
;;; Modifies: AY
copy_to_3buffer:
	lda	(acsrc),y
	sta	b3buffer
	;; Position to the first row of the next character.
	tya
	add	#8
	tay
	lda	(acsrc),y
	sta	b3buffer+1
	lda	#0
	sta	b3buffer+2
	rts
	
;;; Create the lazerline effect. Just ORs the current lazerlight value into the b3buffer.
;;; Input: b3buffer
;;; Output: A=lazerlight_value
lazerlight_3buffer:
	lda	b3buffer
	ora	lazerlight_value
	sta	b3buffer
	lda	b3buffer+1
	ora	lazerlight_value
	sta	b3buffer+1
	lda	b3buffer+2
	ora	lazerlight_value
	sta	b3buffer+2
	rts

;;; Copies the three bytes in the b3buffer into the destination charset.
;;; Input: b3buffer
;;; Ouput: -
;;; Modifies: AX
copy_3buffer_to_destination_charset:
	sty	@Ystore
	ldy	#0
	ldx	#0
@loop:	lda	b3buffer,x
	sta	(acdst),y
	tya
	add	#8
	tay
	inx
	cpx	#3
	bne	@loop
	ldy	#0
	@Ystore=*-1
	rts

;;; Copy and rock'n'ror a 3buffer.
;;; Input:
;;; Output: Y=Y+8
;;; Modifies: AXY
update_animation_line:
	jsr	copy_to_3buffer
	;; rock_and_ror_3buffer
	jsr	lazerlight_3buffer
	jsr	copy_3buffer_to_destination_charset
	rts

;;; Do an animation update.
;;; Input: acsrc, acdst
;;; Modifies: AXY, acsrc, acdst
update_animation_chars:
	ldy	#0
	.repeat	8
	jsr	update_animation_line
	tya
	sub	#7
	tay
	P_inc	acdst
	.endrepeat
	tya
	add	#128-8
	tay
	P_addimm	4*8-8,acdst
	.repeat	8
	jsr	update_animation_line
	tya
	sub	#7
	tay
	P_inc	acdst
	.endrepeat
	rts
	
	.data
animation_sequence_index:
	.byte	0

	.code
animate_char_fontupdate:
	jsr	update_empty_value
	;; Get animation index number
	lda	animation_sequence_index
	lsr			; Divide by four to slow down.
	lsr
	and	#3		; Four animations
	asl			; *2, as these are pointers.
	tay
	;; Setup pointers for source
	lda	animation_pointers,y
	sta	acsrc
	iny
	lda	animation_pointers,y
	sta	acsrc+1
	inc	animation_sequence_index ; Next animation in next frame.
	lda	#<(animate_char_chargenaddr)
	sta	acdst
	lda	#>(animate_char_chargenaddr)
	sta	acdst+1
	jsr	update_animation_chars
	rts

