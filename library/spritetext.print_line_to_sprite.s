
	.importzp	ptr1
	.import	print_char_to_sprite

	.export	print_line_to_sprite
	.exportzp	SPRITETEXT_DESTINATION

	.bss
line_index:	.res	1	; Index into the current line.

	.zeropage
SPRITETEXT_DESTINATION:	.res	2	; Destination pointer.

;;; Copy three characters to the sprite buffer, incrementing all pointers and indices accordingly.
;;; Input: line_index, ptr1, SPRITETEXT_DESTINATION
;;; Output: line_index, SPRITETEXT_DESTINATION
;;; Destroys: A, X, Y
	.code
.proc	print_three_chars_to_sprite
	lda	#3
	sta	ctr
l1:	ldy	line_index
	lda	(ptr1),y	; Get character.
	tay
	lda	SPRITETEXT_DESTINATION ; Destination address.
	ldx	SPRITETEXT_DESTINATION+1
	jsr	print_char_to_sprite
	inc	line_index
	inc	SPRITETEXT_DESTINATION
	dec	ctr
	bne	l1
	rts
	.bss
ctr:	.res	1
.endproc


	.code
print_line_to_sprite:
	sta	ptr1
	stx	ptr1+1
	lda	#0		; Begin with first character.
	sta	line_index
copyl:	jsr	print_three_chars_to_sprite
	lda	#64-3		; 64 bytes per sprite buffer but it has been incremented three times.
	clc
	adc	SPRITETEXT_DESTINATION
	sta	SPRITETEXT_DESTINATION
	bcc	nocarry
	inc	SPRITETEXT_DESTINATION+1
nocarry:
	lda	line_index
	cmp	#24
	bne	copyl
	rts
	
