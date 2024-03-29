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
	.include	"t7d/pseudo/pseudo16.inc"
	.import	animate_char_chargenaddr
	.export	animate_char_fontupdate
	.export animate_char_putat
	.export animate_char_initialise
	.export animate_char_draw_update
	.export animate_char_create
	.export animate_char_frame_update
	.export	charsprite_x
	.macpack generic
	.macpack longbranch

	EMPTY_CHAR_CELL = $FF
	MAX_CHARSPRITES = 18

	.bss
charsprite_x:	.res	MAX_CHARSPRITES
charsprite_y:	.res	MAX_CHARSPRITES

	.zeropage
;;; This is a three byte buffer in the zeropage where a single line of the animation. It is in the zeropage for speed (ROR).
b3buffer:	.res	3
;;; Pointers for all character copy operations.
	;; Source pointer
acsrc:	.res	2
	;; Destination pointer
acdst:	.res	2
acdst2:	.res	2

	.rodata
;;; Pointers to the animation character. Four animations of 2×2 characters.
animation_pointers:
	.word	animate_char_chargenaddr+8*(16*14+0)
	.word	animate_char_chargenaddr+8*(16*14+2)
	.word	animate_char_chargenaddr+8*(16*14+4)
	.word	animate_char_chargenaddr+8*(16*14+6)

	.define screenposfromline $0400,$0400+40*0,$0400+40*1,$0400+40*2,$0400+40*3,$0400+40*4,$0400+40*5,$0400+40*6,$0400+40*7,$0400+40*8,$0400+40*9,$0400+40*10,$0400+40*11,$0400+40*12,$0400+40*13,$0400+40*14,$0400+40*15,$0400+40*16,$0400+40*17,$0400+40*18,$0400+40*19,$0400+40*20,$0400+40*21,$0400+40*22,$0400+40*23,$0400+40*24
spfllo:	.lobytes screenposfromline
spflhi:	.hibytes screenposfromline

	.data
;;; The lazerlight is the vertical line scrolling on the whole screen from left to right. It will be put into the character EMPTY_CHAR_CELL
lazerlight_value:
	.byte	%10000000

	.data
animation_sequence_index:
	.byte	0


	.code

animate_char_initialise:
	lda	#$FF		; Negative value means: no character sprite active.
	ldx	#MAX_CHARSPRITES
@loop:	sta	charsprite_x,x
	sta	charsprite_y,x
	dex
	bpl	@loop
	rts

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

rock_and_ror_3buffer:
	asl
	tax
	beq	@out
@l:	lsr	b3buffer
	ror	b3buffer+1
	ror	b3buffer+2
	dex
	bne	@l
@out:	rts

;;; Copy and rock'n'ror a 3buffer.
;;; Input: A=shift how many multicolour pixels to the right? (≤3)
;;; Output: Y=Y+8
;;; Modifies: AXY
update_animation_line:
	.ifndef	NDEBUG
	cmp	#4
	bcc	@debug
	.byte	2
	@debug:
	.endif
	pha
	jsr	copy_to_3buffer
	pla
	jsr	rock_and_ror_3buffer
	jsr	lazerlight_3buffer
	jmp	copy_3buffer_to_destination_charset

;;; Do an animation update.
;;; Input: acsrc, acdst
;;; Modifies: AXY, acsrc, acdst
update_animation_chars:
	ldy	#0
	.repeat	8
	lda	@wavl
	lsr
	lsr
	and	#3
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
	lda	@wavl
	lsr
	lsr
	and	#3
	jsr	update_animation_line
	tya
	sub	#7
	tay
	P_inc	acdst
	.endrepeat
	inc	@wavl
	rts
@wavl: .byte 0
	
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
	lda	#<(animate_char_chargenaddr)
	sta	acdst
	lda	#>(animate_char_chargenaddr)
	sta	acdst+1
	jsr	update_animation_chars
	rts


;;; Input: X=x-pos [0..42], Y=y-pos (both character positions)
;;; Modifies: AXY, acdst
animate_char_putat:
	.ifndef	NDEBUG
	cpx	#43
	bcc	@nokill
	.byte	$22	 ; KILL
	@nokill:
	.endif
	lda	spfllo,y	; Set up the pointer to the character position.
	sta	acdst
	sta	acdst2
	lda	spflhi,y
	sta	acdst+1
	sta	acdst2+1
	P_addimm	40,acdst2
	txa			; Put x-coordinate into the Accumulator.
	jeq	@single_char
	cmp	#1
	beq	@two_chars
	cmp	#2
	beq	@three_chars
	;; Three characters at positions one, so "␠@AB" and "␠DEF" must be put there. The ␠ is the empty character.
	sub	#3		; Three characters to the left.
	tay			; Move to the index register Y.
	lda	#EMPTY_CHAR_CELL
	sta	(acdst),y
	sta	(acdst2),y
	iny
	cpy	#40
	beq	@maxcol
	lda	#0
	sta	(acdst),y
	lda	#4
	sta	(acdst2),y
	iny
	cpy	#40
	beq	@maxcol
	lda	#1
	sta	(acdst),y
	lda	#5
	sta	(acdst2),y
	iny
	cpy	#40
	beq	@maxcol
	lda	#2
	sta	(acdst),y
	lda	#6
	sta	(acdst2),y
@maxcol:
	rts
@three_chars:
	sub	#2		; Two characters to the left.
	tay			; Move to the index register Y.
	lda	#0
	sta	(acdst),y
	lda	#1
	iny
	sta	(acdst),y
	lda	#2
	iny
	sta	(acdst),y
	P_addimm	40,acdst
	lda	#6
	sta	(acdst),y
	lda	#5
	dey
	sta	(acdst),y
	lda	#4
	dey
	sta	(acdst),y
	rts
@two_chars:
	ldy	#0
	lda	#1
	sta	(acdst),y
	lda	#2
	iny
	sta	(acdst),y
	P_addimm	40,acdst
	lda	#6
	sta	(acdst),y
	lda	#5
	dey
	sta	(acdst),y
	rts
@single_char:
	ldy	#0
	lda	#2
	sta	(acdst),y
	P_addimm	40,acdst
	lda	#6
	sta	(acdst),y
	rts


;;; Draw all the characters sprites at their current positions and then update (move right). Sprites going off the right edge are removed.
;;; This function will only work if the animation_sequence_index is zero otherwise no update is done.
;;; Input: animation_sequence_index
;;; Output: -
;;; Modifies: AXY
animate_char_draw_update:
	lda	animation_sequence_index
	cmp	#1
	bne	@out
	lda	#0
	sta	@acidx		; Set the current index.
@loop:	ldy	@acidx		; Get current index into Y.
	cpy	#MAX_CHARSPRITES ; Maximum reached?
	beq	@out
	lda	charsprite_x,y	; X-coordinate
	bmi	@inactive	; This sprite is inactive.
	tax			; Store X-coordinate in X.
	add	#1		; Go to next position.
	cmp	#43		; Maximal right column?
	bne	@still_active
	lda	#$ff		; -1, therefore inactive
@still_active:
	sta	charsprite_x,y	; Store new position.
	lda	charsprite_y,y	; y-coordinate
	tay			; Into Y register.
	jsr	animate_char_putat
@inactive:
	inc	@acidx		; Next, please.
	bne	@loop		; Actually the loop ends earlier, see above.
@out:
	rts
@acidx:	.byte	0

animate_char_frame_update:
	lda	animation_sequence_index ; Get current index.
	add	#1			 ; Add one.
	and	#16-1			 ; Four animations times 1/4 speed.
	sta	animation_sequence_index
	jsr	animate_char_fontupdate
	.ifndef	NDEBUG
	inc	$d020
	.endif
	jsr	animate_char_draw_update
	rts

;;; Create a new sprite.
;;; Input: A=new Y position.
;;; Output: -
;;; Modifies: AXY
animate_char_create:
	.ifndef	NDEBUG
	cmp	#25
	bcc	@out
	.byte 2,"animate_char_create"
	@out:
	.endif
	tay			; Store new position.
	ldx	#0
@loop:	lda	charsprite_x,x	; Is the sprite active?
	bpl	@active
	tya			; Get stored y position.
	sta	charsprite_y,x	; Store the new Y position.
	lda	#0
	sta	charsprite_x,x	; Clear the sprite X position.
	rts			; END
@active:
	inx
	cpx	#MAX_CHARSPRITES
	bne	@loop
	;; No free slot found.
	rts
