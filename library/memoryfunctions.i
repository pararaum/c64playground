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
	inc	@l1+2		; increment hi of src
	inc	@l1+5		; increment hi or dst
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

;;; Clear a memory area macro.
;;; The memory is cleared with zeros. Warning! Only usable once.
;;; Modifies: A/X/Y
	.macro bzero_once_macro	dest, size
	.if	>size <> 0
	lda	#0
	tax
	tay
@l1:	sta	dest,x
	inx
	bne	@l1
	inc	@li+2		; Increment hi of dest
	iny
	cpy	#>size
	bne	@l1
	.endif
	.if	<size <> 0
	.if	<size = 1
	sta	dest+(size & $ff00)
	.else
@l2:	sta	dest+(size & $ff00),x
	inx
	cpx	#<size
	bne	@l2
	.endif
	.endif
	.endmacro
