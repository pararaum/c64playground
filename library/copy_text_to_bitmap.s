	.include	"zeropage.inc"

	.export	copy_text_to_bitmap

txtsrc=ptr1
chrsrc=ptr2
bmpdst=ptr3
chrcpyptr=ptr4

	.bss
charcounter:	.res	2	; Counter for the 1000 characters.

	.code
.proc	copy_char
	ldy	#0
	sty	chrcpyptr+1	; HI
	lda	(txtsrc),y
	.REPEAT	3
	asl
	rol	chrcpyptr+1
	.ENDREPEAT
	sta	chrcpyptr		; ptr2=8*character
	lda	chrcpyptr+1
	clc
	adc	chrsrc+1	; HI of character ROM address or font.
	sta	chrcpyptr+1
	ldy	#8
l1:	lda	(chrcpyptr),y
	sta	(bmpdst),y
	dey
	bpl	l1
	rts
.endproc
	
copy_text_to_bitmap:
	lda	#<1000
	sta	charcounter
	lda	#>1000
	sta	charcounter+1
convloop:
	jsr	copy_char
	;; txtsrc+=1
	inc	txtsrc		; LO of txtsrc
	bne	nocarry2
	inc	txtsrc+1	; HI of txtsrc
nocarry2:
	;; bmpdst+=8
	lda	bmpdst
	clc
	adc	#8
	sta	bmpdst
	bcc	nocarry
	inc	bmpdst+1
nocarry:
	lda	charcounter
	bne	nounderflow
	dec	charcounter+1
	bpl	nounderflow	; If we reach $FFxx then the loop is finished.
	rts
nounderflow:
	dec	charcounter
	jmp	convloop
