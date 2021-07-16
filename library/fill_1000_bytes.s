
	.importzp	ptr1

	.export	fill_1000_bytes

	.code
.proc fill_1000_bytes
	sta	ptr1
	stx	ptr1+1
	tya
	ldy	#0
	ldx	#3-1
l1:	sta	(ptr1),y
	dey
	bne	l1
	inc	ptr1+1
	dex
	bpl	l1
l2:	sta	(ptr1),y
	iny
	cpy	#<(1000-3*256)
	bne	l2
	rts
.endproc
