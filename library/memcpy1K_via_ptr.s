
	.importzp	ptr1,ptr2

	.export	memcpy1K_via_ptr

.PROC	memcpy1K_via_ptr
	ldy	#0		; Initialise index register.
	.REPEAT	4, I
	.scope
copylabel:
	lda	(ptr1),y
	sta	(ptr2),y
	dey
	bne	copylabel
	inc	ptr1+1
	inc	ptr2+1
	.endscope
	.ENDREPEAT
	rts
.ENDPROC
