	.include	"t7d/sprite/spritepad.i"

	.importzp	ptr1, ptr2
	
copy_spad_data:
	sta	destination
	stx	destination+1
	sty	ptr2+1		; Hi of offset into spritepad data.
	lda	#0		; Low of...
	lsr	ptr2+1		; Six bits to the left..., you know.
	ror
	lsr	ptr2+1
	ror
	clc
	adc	#SPRITEPADDATAOFFSET
	adc	ptr1		; Spritepad data pointer.
	sta	ptr2
	bcc	no_hi_inc
	inc	ptr2+1
no_hi_inc:
	lda	ptr1+1
	clc
	adc	ptr2+1
	sta	ptr2+1
	ldy     #63-1
loop:  lda     (ptr2),y
        sta     destination,y
destination=*-2
        dey
        bpl     loop
	rts
