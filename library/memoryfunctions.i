;;; -*- mode: asm -*-
;;; Routines to handle memory functions.


;;; Memory copy macro, copies memory downwards.
;;; The memory is copied from src to destination. The number of bytes must be given in size.
;;; Modifies: A/X/Y
	.macro	memcpy_down_macro	src, dest, size
	.local	@l1,@l2
	.if	>size <> 0
	ldy	#0
	ldx	#0
@l1:	lda	src,x
	sta	dest,x
	inx
	bne	@l1
	iny
	cpy	#>size
	bne	@l1
	.endif
	.if	<size <> 0
	.if	<size = 1
	lda	src+(size & $ff00)
	sta	dest+(size & $ff00)
	.else
	ldx	#0
@l2:	lda	src+(size & $ff00),x
	sta	dest+(size & $ff00),x
	inx
	cpx	#<size
	bne	@l2
	.endif
	.endif
	.endmacro
