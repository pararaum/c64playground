	.include	"moiety.wiggle.i"
	.include	"zeropage.inc"

MAXIMUM_WIGGLE_FRAMES=4		; Maximum is 32.

	.bss
;;; Number of chars to wiggle, counted down until zero.
number_wiggle_chars:		.res	1
;;; Current index into the wiggle table
current_wiggle_table_index:	.res	1

	.data
wiggle_offset:	.byte	0

	.code
;;; A=destination character
;;; X=current index
.proc	update_char
	sta	ptr1		; Destination LO.
	lda	#0		; Destination HI will be stored here.
	.repeat	3		; Multiply by 8.
	 asl	ptr1
	 rol
	.endrepeat
	sta	ptr1+1		; Store destinatin HI in pointer.
	lda	#<WIGGLE_DESTINATION_CHARSET
	clc
	adc	ptr1
	sta	ptr1
	lda	#>WIGGLE_DESTINATION_CHARSET
	clc
	adc	ptr1+1
	sta	ptr1+1
	;;  ptr1 now contains the destination character address.
	lda	WIGGLE_SOURCE_LO,x
	clc
	adc	wiggle_offset
	sta	ptr2
	lda	WIGGLE_SOURCE_HI,x
	adc	#0
	sta	ptr2+1
	;; ptr2 now contains the source.
	ldy	#7
l1:	lda	(ptr2),y
	sta	(ptr1),y
	dey
	bpl	l1
	rts
.endproc

.proc update_wiggle
	sta	number_wiggle_chars
	lda	#0
	sta	current_wiggle_table_index
wiggle_loop:
	ldx	current_wiggle_table_index
	lda	WIGGLE_CHARS,x
	jsr	update_char
	inc	current_wiggle_table_index
	dec	number_wiggle_chars
	bne	wiggle_loop
	lda	wiggle_offset
	clc
	adc	#8
	cmp	#8*MAXIMUM_WIGGLE_FRAMES
	bne	noclear
	lda	#0
noclear:
	sta	wiggle_offset
	rts
.endproc
