;;; -*- mode: asm -*-
;;; Routines to handle memory functions.

;;; Copy exactly 1K of memory in an inlined function.
;;; Input: source, destination
;;; Output: -
;;; Modifies: A,X
	.macro	Memcpy1KInline	source,destination
	.local	 @loop
	ldx	#0
@loop:
	.repeat	4,I
	  lda	source+I*$100,x
	  sta	destination+I*$100,x
	.endrepeat
	dex
	bne	@loop
	.endmacro

;;; Copy exactly 1K of memory from ptr1 to ptr2
;;; Input: ptr1, ptr2
;;; Modifies: A,Y
;;; Output: ptr1+=$0400, ptr2+=$0400
	.import	memcpy1K_via_ptr

;;; Copy exactly 1K of memory (see above) as a macro.
	.macro	Memcpy1KViaPtr	source, destination
	lda	#<(source)
	sta	ptr1
	lda	#>(source)
	sta	ptr1+1
	lda	#<(destination)
	sta	ptr2
	lda	#>(destination)
	sta	ptr2+1
	jsr	memcpy1K_via_ptr
	.endmacro

;;; Memory copy macro for up to 255 bytes.
;;;
;;; Input:
;;;	srcaddr=pointer to the source,
;;;	size=Number of bytes to copy
;;;	dstaddr=pointer to the destination,
;;; Modifies: A, X
;;; Output: -
	.macro	SmallMemCpy	srcaddr,size,dstaddr
	.local	@loop
	ldx	#<(size)
@loop:	lda	0+(srcaddr)-1,x
	sta	0+(dstaddr)-1,x
	dex
	bne	@loop
	.endmacro


;;; Memory copy macro for up to 256 bytes with pointer initialisation.
;;; The source and destination pointers are not changed and can be reused.
;;;
;;; WARNING! Remember to put a '#' in front of the size if the value
;;;	is fixed. Otherwise the end value is fetched from memory.
;;;
;;; Input:
;;;	srcaddr=pointer to the source,
;;;	dstaddr=pointer to the destination,
;;;	size=#number of bytes or use variable/memory location,
;;;	tmpptr1=a pointer in the zeropage,
;;;	tmpptr2=a pointer in the zeropage
;;; Modifies: A, Y
;;; Output: -
	.macro	smallmemcpy_macro_winit	scraddr, dstaddr, size, tmpptr1, tmpptr2
	lda	#<(scraddr)
	sta	tmpptr1
	lda	#>(scraddr)
	sta	tmpptr1+1
	lda	#<(dstaddr)
	sta	tmpptr2
	lda	#>(dstaddr)
	sta	tmpptr2+1
	smallmemcpy_macro	tmpptr1, tmpptr2, size
	.endmacro


;;; Memory copy macro for up to 256 bytes.
;;; The source and destination pointers are not changed and can be reused.
;;; Input: srcptr=pointer to the source,
;;; 	dstptr=pointer to the destination,
;;; 	size=number of bytes (#… is immediate, otherwise get from memory)
;;; Modifies: A, Y
;;; Output: -
	.macro	smallmemcpy_macro	srcptr, dstptr, size
	.local	@l1
;	.if (.not .match (.left (1, {arg}), #))
;	 .if size > 256
;	  .error	More than 256 Bytes to copy!
;	 .endif
;	.endif
	ldy	#0		; Set up index register.
@l1:	lda	(srcptr),y
	sta	(dstptr),y
	iny			; Increment index register.
	.if (.match (.left (1, {size}), #))
	;; Immediate
	;; Reached the end? For 256 Bytes this waits for the overflow.
	 cpy	#(.right (.tcount ({size})-1, {size}))
	.else
	;; Get from memory
	 cpy	size
	.endif
	bne	@l1
	.endmacro

;;; Memory copy macro, copies memory downwards.
;;; The memory is copied from src to destination. The number of bytes must be given in size.
;;; Modifies: A/X/Y
	.macro	memcpy_down_macro	src, dest, size
	.local	@l1,@l2
	.if	>(size) <> 0
	ldy	#0
	ldx	#0
@l1:	lda	src,x
	sta	dest,x
	inx
	bne	@l1
	inc	@l1+2		; increment hi of src
	inc	@l1+5		; increment hi or dst
	iny
	cpy	#>(size)
	bne	@l1
	.endif
	.if	<(size) <> 0
	.if	<(size) = 1
	lda	src+((size) & $ff00)
	sta	dest+((size) & $ff00)
	.else
	ldx	#0
@l2:	lda	src+((size) & $ff00),x
	sta	dest+((size) & $ff00),x
	inx
	cpx	#<(size)
	bne	@l2
	.endif
	.endif
	.endmacro



;;; Memory copy macro, copies memory upwards.
;;; The memory is copied from src to destination. The number of bytes must be given in size.
;;; Input: from, to=two pointers in ZP to use, src=source address, dest=destination address, size=size in bytes
;;; Reference: http://6502.org/source/general/memory_move.html
;;; Modifies: A/X/Y, ptr1, ptr2
	.macro	memcpy_ptr_up_macro	fromptr, toptr, src, dest, size
	.local	@mu1,@mu2,@mu3
	lda	#<(src)
	sta	fromptr
	lda	#>(src)
	sta	fromptr+1
	lda	#<(dest)
	sta	toptr
	lda	#>(dest)
	sta	toptr+1
	ldx	#>(size)	; the last byte must be moved first
	clc			; start at the final pages of FROMPTR and TOPTR
	txa
	adc	fromptr+1
	sta	fromptr+1
	clc
	txa
	adc	toptr+1
	sta	toptr+1
	inx	     ; allows the use of BNE after the DEX below
	ldy	#<(size)
	beq	@mu3
	dey	     ; move bytes on the last page first
	beq	@mu2
@mu1:
	lda	(fromptr),y
	sta	(toptr),y
	dey
	bne	@mu1
@mu2:
	lda	(fromptr),y	; handle Y = 0 separately
	sta	(toptr),y
@mu3:
	dey
	dec	fromptr+1	; move the next page (if any)
	dec	toptr+1
	dex
	bne	@mu1
	.endmacro


;;; Function to copy the memory upwards
;;; Input: ptr1=pointer to src, ptr2=destination pointer, A/X=size
;;; Output: X=0
;;; Modifies: ptr1, ptr2, A,X,Y
	.import	memcpy_up


;;; Function to copy the memory downwards
;;; Input: ptr1=pointer to src, ptr2=destination pointer, A/X=size
;;; Output: -
;;; Modifies: ptr1, ptr2, A,X,Y
	.import	memcpy_down



;;; Function to copy the memory downwards with inline parameters
;;; Input: two words after JSR
;;;	- destination
;;;	- size in bytes
;;; Output: -
;;; Modifies: ptr1, ptr2, ptr3, tmp1, A,X,Y
	.import	copy_memory_downwards_ip


;;; Helper Macro to easily use the copy memory function with in-place parameters with files.
;;; Input:
;;;	- destination: destination address to copy data to
;;;	- fname: file name to read data from
;;;	- skip(optional): how many bytes to skip at the beginning of the file
;;;	- length(optional): how many bytes to include from the file
.macro	CopyFileIntoMemoryDown destination, fname, skip, length
	.local	data, dataend
	jsr	copy_memory_downwards_ip
	.word	destination
	.word	dataend-data
data:
	.if .paramcount = 4
	 .incbin	fname, skip, length
	.elseif .paramcount = 3
	 .incbin	fname, skip
	.else
	 .incbin	fname
	.endif
dataend:
.endmacro


;;; Clear a memory area macro.
;;; The memory is cleared with zeros. Warning! Only usable once.
;;; Modifies: A/X/Y
	.macro bzero_once_macro	dest, size
	.local	@l1,@l2
	.if	>(size) <> 0
	lda	#0
	tax
	tay
@l1:	sta	dest,x
	inx
	bne	@l1
	inc	@l1+2		; Increment hi of dest
	iny
	cpy	#>(size)
	bne	@l1
	.endif
	.if	<(size) <> 0
	.if	<(size) = 1
	sta	dest+((size) & $ff00)
	.else
@l2:	sta	0+(dest+((size) & $ff00)),x
	inx
	cpx	#<(size)
	bne	@l2
	.endif
	.endif
	.endmacro


;;; Clear a memory area macro.
;;; The memory is cleared with zeros. This code set the pointer at the
;;;	beginning, therefore it can be called in a loop or a function
;;;	multiple times.
;;; Modifies: A/X/Y
	.macro BZeroMacro	dest, size
	.local	@l1,@l2,@destptr
	.if	>(size) <> 0
	lda	#<(dest)
	sta	@destptr
	lda	#>(dest)
	sta	@destptr+1
	lda	#0
	tax
	tay
@l1:	sta	0+(dest),x
	@destptr=*-2
	inx
	bne	@l1
	inc	@destptr+1		; Increment hi of dest
	iny
	cpy	#>(size)
	bne	@l1
	.else
	lda	#0
	tax
	.endif
	.if	<(size) <> 0
	.if	<(size) = 1
	sta	(dest)+((size) & $ff00)
	.else
@l2:	sta	0+((dest)+((size) & $ff00)),x
	inx
	cpx	#<(size)
	bne	@l2
	.endif
	.endif
	.endmacro

;;; Clear a memory block.
;;; Input: ptr1=destination address, A/X=size in bytes
;;; Output: A=0, X=0
;;; Modifies: A,X,Y,ptr1
	.global	clear_memory

;;; Clear a memory block, using program memory for parameters.
;;; Input: offset after JSR: +0 destination address, +2 size in bytes
;;; Output: A=0, X=0
;;; Modifies: A,X,Y,ptr1,ptr2
	.global	clear_memory_progmem


;;; Copy memory with strides, this function needs several variables. Use this function to copy e.g. a block of PETSCII chars from one frame into another.
;;; memcpy_strided_srcwidth, memcpy_strided_srcheight, memcpy_strided_srcstride, memcpy_strided_dststride
;;; Input: memcpy_strided_srcwidth, memcpy_strided_srcheight, memcpy_strided_srcstride, memcpy_strided_dststride
;;; Modifies: A,X,Y,ptr1,ptr2
	.global	memcpy_strided	; Function.
	.global	memcpy_strided_srcwidth ; Width of the rectangle.
	.global	memcpy_strided_srcheight ; Height of the rectangle.
	.global	memcpy_strided_srcstride ; Stride (width of page/frame) of source.
	.global	memcpy_strided_dststride ; Stride (width of page/frame) of destination.
