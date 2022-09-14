NOBCHARS=7
BULLETSPEED=1

	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/copy_chargen.i"

	.import	__SCREEN0_START__
	.import	CHARGEN_DESTINATION

	.export	update_takeover_bulleteffect
	.export	init_takeover_bulleteffect

	.zeropage
scrtptr:	.res	2

	.data
topleft:	.byte	0
skipcounter:	.byte	0	; Skip some frames before next update.

	.bss
chrcol:	.res	1		; Use this value as the foreground colour for the chars.
linecounter:	.res	1	; We need a counter to handle the 25 lines and stop then.

	.rodata
bulletchars:
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00001000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;; 
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00011000
	.byte	%00011000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	;; 
	.byte	%00000000
	.byte	%00000000
	.byte	%00011000
	.byte	%00111100
	.byte	%00111100
	.byte	%00011000
	.byte	%00000000
	.byte	%00000000
	;; 
	.byte	%00000000
	.byte	%00011000
	.byte	%00111100
	.byte	%01111110
	.byte	%01111110
	.byte	%00111100
	.byte	%00011000
	.byte	%00000000
	;; 
	.byte	%00011000
	.byte	%00111100
	.byte	%01111110
	.byte	%11111111
	.byte	%11111111
	.byte	%01111110
	.byte	%00111100
	.byte	%00011000
	;; 
	.byte	%01111110
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%01111110
	;; 
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111


	.code
init_takeover_bulleteffect:
	sta	chrcol
	jsr	copy_chargen
	ldx	#NOBCHARS*8-1
@l1:	lda	bulletchars,x
	sta	CHARGEN_DESTINATION+(256-NOBCHARS)*8,x
	dex
	bpl	@l1
	SetChargenAddress	CHARGEN_DESTINATION
	rts


	.code
;;; Input: Y=Starting Y-position, scrtptr=pointer to line
draw_bullets:
	tya
	pha
	lda	#$ff-NOBCHARS+1
@l1:
	cpy	#40
	bcs	@nowrite
	sta	(scrtptr),y
	pha			; Current character code.
	lda	scrtptr		; Store old screen pointer, LO.
	pha
	clc
	adc	#<($D800-__SCREEN0_START__) ; Adjust to colour ram.
	sta	scrtptr
	lda	scrtptr+1
	pha
	adc	#>($D800-__SCREEN0_START__) ; Adjust to colour ram, carry ok from LO addition.
	sta	scrtptr+1
	lda	chrcol		; Get colour information.
	sta	(scrtptr),y	; Change the colour of the character.
	pla			; Restore old pointer.
	sta	scrtptr+1
	pla
	sta	scrtptr
	pla			; Get current character code.
@nowrite:
	clc
	adc	#1
	bpl	@out
	dey
	bpl	@l1
@out:	pla
	tay
	rts


	.code
update_takeover_bulleteffect:
	dec	skipcounter
	bpl	@skip
	lda	#BULLETSPEED
	sta	skipcounter
	lda	#<__SCREEN0_START__
	sta	scrtptr
	lda	#>__SCREEN0_START__
	sta	scrtptr+1
	lda	#25
	sta	linecounter
	ldy	topleft
@l1:	jsr	draw_bullets
	lda	scrtptr		; Increment screen pointer to next line.
	clc
	adc	#40
	sta	scrtptr
	lda	scrtptr+1
	adc	#0
	sta	scrtptr+1
	dec	linecounter
	beq	@finished
	dey
	bpl	@l1
@finished:
	lda	topleft		; What is the position of the top left window?
	cmp	#2*39-8-1
	beq	@final
	inc	topleft
@skip:
	sec
	rts
@final:
	clc
	rts

