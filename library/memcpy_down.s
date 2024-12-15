	.export	memcpy_down

	.importzp	ptr1, ptr2

;;; A/X=size
;;; Out: X=0
;;; Modifies: A, X, Y, ptr1, ptr2, tmp1
	.proc	memcpy_down
	ldy	#0
	sta	self_modifying	; LO of size.
	cpx	#0
	beq	lt256		; Less than 256 Bytes to copy.
l1:	lda	(ptr1),y
	sta	(ptr2),y
	iny
	bne	l1
	inc	ptr1+1		; increment hi of src
	inc	ptr2+1		; increment hi or dst
	dex
	bne	l1
lt256:	lda	self_modifying	; Check if LO of size different from zero.
	beq	out		; Finished, we have copied whole pages.
l2:	lda	(ptr1),y
	sta	(ptr2),y
	iny
	cpy	#$FF
	self_modifying=*-1
	bne	l2
out:	rts
.endproc
