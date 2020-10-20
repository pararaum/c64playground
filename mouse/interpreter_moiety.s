;;; Read mouse and interpret results.
	.import SPRITEX
	.import SPRITEY
	.import SPRITEMSB

	.export	mouse_init
	.export	mouse

	PADDLEX	= $d419
	PADDLEY = $d41a

	.bss
oldpotx:	.res	1	; Old X potentiometer value
oldpoty:	.res	1	; Old Y potentiometer value
	
	.code
;;; Initialise mouse engine.
;;; Input: -
;;; Output: -
;;; Modifies: -
mouse_init:
	pha
	lda	PADDLEX
	sta	oldpotx
	lda	PADDLEY
	sta	oldpoty
	pla
	rts


;;; Read mouse and update mouse pointer coordinates.
;;; Input: -
;;; Output: -
;;; Modifies: AXY
mouse:
	lda	PADDLEX	   ; get delta values for x
	ldy	oldpotx	   ; last potentiometer value
	jsr	calculate_movement
	sty	oldpotx
	;; modify low order xposition
	clc
	adc	SPRITEX
	sta	SPRITEX
	txa
	adc	#$00
	and	#%00000001
	eor	$D010
	sta	$D010
	;; Y
	lda	PADDLEY	   ; get delta value for y
	ldy	oldpoty
	jsr	calculate_movement
	sty	oldpoty
	;;  modify y position ( decrease y for increase in pot )
	sec	
	eor	#$ff
	adc	SPRITEY
	sta	SPRITEY
	rts

;;; Calculate the movement difference from the potentiometer reading.
;;; Input: A=current potentiometer value, Y=old potentiometer value
;;; Output: XA=delta value for position, Y=current potentiometer value to be stored.
;;; Modifies: AXY
calculate_movement:
	sty	@tempoldvalue
	sta	@tempnewvalue
	ldx	#0
	sec
	sbc	@tempoldvalue
	and	#%01111111	
	cmp	#%01000000
	bcs	@l1
	lsr
	beq	@slutt
	ldy	@tempnewvalue
	rts
@l1:
	ora	#%11000000
	cmp	#$ff
	beq	@slutt
	sec
	ror
	ldx #$ff
	ldy @tempnewvalue
	rts
@slutt:
	lda #0
	rts
	.bss
@tempoldvalue:	.res	1
@tempnewvalue:	.res	1

