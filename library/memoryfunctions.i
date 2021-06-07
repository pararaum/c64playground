;;; -*- mode: asm -*-
;;; Routines to handle memory functions.

;;; Memory copy macro for up to 256 bytes.
;;; The source and destination pointers are not changed and can be reused.
;;; Input: srcptr=pointer to the source, dstptr=pointer to the destination, size=number of bytes
;;; Modifies: A, Y
;;; Output: -
	.macro	smallmemcpy_macro	srcptr, dstptr, size
	.local	@l1
	.if	size > 256
	 .error	More than 256 Bytes to copy!
	.endif
	ldy	#0		; Set up index register.
@l1:	lda	(srcptr),y
	sta	(dstptr),y
	iny			; Increment index register.
	cpy	#<size		; Reached the end? For 256 Bytes this waits for the overflow.
	bne	@l1
	.endmacro

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
