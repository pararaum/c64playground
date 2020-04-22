
	.export	animationfont
	.export animationfont_end
	.export	wavymation_copy_font
	.export wavymation_animate_font
	.export wavymation_generate_image

	.import wavymation_chargenptr
	.import wavymation_screenptr

;;; Generate the image from a bitmap. The image must be given via A/X and will be written to the wavymation_screenptr. The image must have a resolution of 40*25.
;;; Input: A(lo)/X(hi)=pointer to image
;;; Modifies: A/X/Y
	.code
wavymation_generate_image:
	sta	@srcptr		; Set the source pointer to the information given in A and X.
	stx	@srcptr+1
	;; Prepare the desination to write to.
	lda	#<wavymation_screenptr
	sta	@destpointer
	lda	#>wavymation_screenptr
	sta	@destpointer+1
	lda	#25-1
	sta	wavy_rowcounter
	;; #
@lscreen:
	lda	wavy_rowcounter
	sta	wavy_columnphase	; Initialise the phase.
	ldy	#40/8-1		; 40 columns, eight "pixels" per byte.
@lrow:
	jsr	@getch
	ldx	#8		; Eight pixels to come.
@lchar:
	asl			; Put next pixel in carry.
	jsr	@draw_pxl
	inc	wavy_columnphase
	dex
	bne	@lchar
	dey			; Next byte (countdown) in row.
	bpl	@lrow
	dec	wavy_rowcounter	; Countdown in rows.
	bpl	@lscreen
	rts

@getch:				; Get the next character/byte.
	lda	$FFFF		; Get the next byte (self-modifying code).
	@srcptr=*-2
	inc	@srcptr
	bne	@s2
	inc	@srcptr+1
@s2:	rts

@draw_pxl:			; Draw a pixel: carry set = set otherwise zero.
	pha
	lda	#0		; Carry is unchanged.
	bcc	@sd		; Carry clear? So no Pixel.
	ora	wavy_columnphase	; Otherwise OR the current value of the wave.
	;; Now modulus operation.
@ml:	sec
	sbc	#12
	bcs	@ml		; Modulus loop.
	adc	#12
	clc
	adc	#1		; Visible characters begin at one.
@sd:	sta	$FFFF		; Self-modifying code.
	@destpointer=*-2
	inc	@destpointer
	bne	@s1
	inc	@destpointer+1
@s1:	pla
	rts
	.data
wavy_columnphase:	.byte	0
wavy_rowcounter:	.byte	0

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

	.code
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
	
