	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/theatre.i"
	.include	"t7d/theatre/sprite_animation.i"

	.importzp	ptr1, ptr2

	.export	theatre_animate_sprt
	.export	theatre_animate_8

	.code

;.proc	increment_frame
;	
;	rts
;.endproc

.proc	theatre_animate_sprt
	ldy	#0
	lda	(ptr1),y	; load current
	bpl	not_yet
	iny			; Y=1
	lda	(ptr1),y	; load speed
	dey
	sta	(ptr1),y	; and store into current
	ldy	#SpriteAnimationEntry::animation_current
	lda	(ptr1),y	; current animation frame
	sta	THEATRE_SPRT_PTR,x
	iny			; Y=3, ergo delta
	clc
	adc	(ptr1),y	; Add delta
	dey			; Y=2, ergo current frame
	sta	(ptr1),y	; store new frame
	ldy	#5		; Y=5, ergo max
	cmp	(ptr1),y	; Maximum reached?
	bcc	not_max
	beq	not_max
	dey			; Y=4, minimum value
	lda	(ptr1),y
	ldy	#2		; Y=2, ergo current
	sta	(ptr1),y
not_max:
	ldy	#1		; Y=1, ergo animation speed
	lda	(ptr1),y
	dey			; Y=0, current speed value
not_yet:
	sec
	sbc	#1
	sta	(ptr1),y
	rts
.endproc

.proc	theatre_animate_8
	lda	ptr1
	ora	ptr1+1
	beq	null
	ldx	#0
loop:	jsr	theatre_animate_sprt
	P_addimm	7, ptr1
	inx
	cpx	#8
	bne	loop
null:	rts
.endproc
	
