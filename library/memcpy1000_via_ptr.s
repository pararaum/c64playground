
	.importzp	ptr1,ptr2

	.export	memcpy1000_via_ptr

.PROC	memcpy1000_via_ptr
	ldy	#0		; Initialise index register.
	.REPEAT	3
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
	;; (- 1000 (* 256 3))232
	ldy	#232-1
l1:	lda	(ptr1),y
	sta	(ptr2),y
	dey
	bne	l1
	;; Y=0 here, we need to copy the final byte. Faster than using CPY and uses only four bytes more.
	lda	(ptr1),y
	sta	(ptr2),y
	rts
.ENDPROC
