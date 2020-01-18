;;; Copy a two text lines into the sprite buffer.
;;; Input: LINEPTR
;;; Output: DATAPTR, LINEPTR=LINEPTR+48
;;; Destroys: A, X, Y, LINEIDX, DATAPTR, LINEPTR
copy_2_lines_to_sprite:
	lda	#<SPRITE_DATAAREA
	sta	DATAPTR
	lda	#>SPRITE_DATAAREA
	sta	DATAPTR+1
	jsr	copy_line_to_sprite
	lda	#24		; Increment line pointer by 24 bytes.
	clc
	adc	LINEPTR
	sta	LINEPTR
	lda	#0
	adc	LINEPTR+1
	sta	LINEPTR+1
	lda	#<(SPRITE_DATAAREA+9*3)
	sta	DATAPTR
	lda	#>(SPRITE_DATAAREA+9*3)
	sta	DATAPTR+1
	jsr	copy_line_to_sprite
	lda	#24		; Increment line pointer by 24 bytes.
	clc
	adc	LINEPTR
	sta	LINEPTR
	lda	#0
	adc	LINEPTR+1
	sta	LINEPTR+1
	rts
	

;;; Copy a whole line into the sprite buffer.
;;; Input: LINEPTR, DATAPTR
;;; Output: DATAPTR
;;; Destroys: A, X, Y, LINEIDX
copy_line_to_sprite:
	lda	#0
	sta	LINEIDX
.copyl:	jsr	copy_three_chars_to_sprite
	lda	#64-3
	clc
	adc	DATAPTR
	sta	DATAPTR
	bcc	.l60
	inc	DATAPTR+1
.l60:
	lda	LINEIDX
	cmp	#24
	bne	.copyl
	rts
	

;;; Copy three characters to the sprite buffer, incrementing all pointers and indices accordingly.
;;; Input: LINEIDX, LINEPTR, DATAPTR
;;; Output: LINEIDX, DATAPTR
;;; Destroys: A, X, Y
copy_three_chars_to_sprite:
	lda	#3
	sta	.ctr
.l1	ldy	LINEIDX
	lda	(LINEPTR),y
	ldx	DATAPTR
	ldy	DATAPTR+1
	jsr	copy_char_to_sprite
	inc	LINEIDX
	inc	DATAPTR
	dec	.ctr
	bne	.l1
	rts
.ctr:	.byte	0


;;; Copy a single character to the sprite buffer.
;;; Input: A=character, X/Y=pointer to target
;;; Output: -
;;; Destroys: A, X, Y
copy_char_to_sprite:
	stx	.tgt+1
	sty	.tgt+2
	ldx	#0
	stx	.src+2
	asl
	rol	.src+2
	asl
	rol	.src+2
	asl
	rol	.src+2
	adc	#<CHARSET
	sta	.src+1
	lda	#>CHARSET
	adc	.src+2
	sta	.src+2
	ldy	#7
	ldx	#7*3
.src:	lda	$EAEA,y
.tgt:	sta	$EAEA,x
	dex
	dex
	dex
	dey
	bpl	.src
	rts


;;; Set the sprite positions consecutive to each other, eight sprite in a row.
;;; Input: Y=y-position, A/X=x-position (A is high)
;;; Output: -
;;; Destroys: A, X, Y
position_sprite:
	sta	.xpos+1		; Store x-position.
	stx	.xpos
	tya			; y-position
	ldy	#1		; Set all eight y-positions.
.l1:	sta	$d000,y
	iny
	iny
	cpy	#15+2
	bne	.l1
	;; Now set x-positions.
	ldy	#0		; 0-th sprite.
.l2:	lda	.xpos+1		; Get hight byte of sprite.
	lsr			; Shift to the right, which means that carry is cleared if less than 256.
	ror	.msbs		; Shift msbs from the right.
	lda	.xpos		; Get x-position, low.
	sta	$d000,y		; Store position.
	clc			; Increment x-position by 24.
	adc	#24
	bcc	.noover
	inc	.xpos+1
.noover:
	sta	.xpos
	iny
	iny
	cpy	#14+2
	bne	.l2
	lda	.msbs
	sta	$d010
	rts
.xpos:	.word	$6060
.msbs:	.byte	$60	


;;; Set all sprite pointers to incremental buffers.
;;; Input: A=sprite buffer number, SPRITE_POINTER
;;; Output: A=incremented by 8
;;; Destroys: A, X
set_sprite_pointer:
	ldx	#0
.l1:	sta	SPRITE_POINTER,x
	clc
	adc	#1
	inx
	cpx	#8
	bne	.l1
	rts


;;; Set the colour of all sprites to the value in A.
;;; Input: a=colour
;;; Output: -
;;; Destroys: A
colour_sprites:
	stx	.savex+1
	ldx	#7
.l1:	sta	$d027,x
	dex
	bpl	.l1
.savex:	ldx	#0
	rts
