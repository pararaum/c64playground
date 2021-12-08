
	.import	SPRITETEXT_CHARGEN

	.export	print_char_to_sprite

print_char_to_sprite:
	sta	DESTINATION	; Set destination pointer.
	stx	DESTINATION+1
	ldx	#0		; Clear high byte of source.
	stx	SOURCE+1
	tya			; Transfer character into A.
	asl			; Multiply by eight.
	rol	SOURCE+1	; And take care of higher bits moved out.
	asl
	rol	SOURCE+1
	asl
	rol	SOURCE+1
	;; Now A has the lower bits of char*8.
	;; C=0 as high byte of source was initialised by zero.
	adc	#<SPRITETEXT_CHARGEN ; Adjust for font.
	sta	SOURCE
	lda	#>SPRITETEXT_CHARGEN
	adc	SOURCE+1
	sta	SOURCE+1
	ldy	#8-1		; Eight bytes to copy.
	ldx	#7*3
l1:	lda	$EAEA,y
	SOURCE=*-2
	sta     $EAEA,x
	DESTINATION=*-2
	dex
	dex
	dex
	dey
	bpl	l1
	rts
