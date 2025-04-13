; -*- mode: asm -*-
; Macros and Functions for handling pointers. This will overlap with
; the pseudo16.inc macros partially but it makes it possible to
; transport a semantic meaning by using the appropriate macro
; etc.

;;; Test if a pointer is a zero pointer.
;;; Input: ptr=which pointer to use
;;; Output: Z=1: pointer is zero, Z=0: pointer is non-zero
;;; Modifies: A
        .macro  PointerTestZero ptr
        lda     ptr
        ora     ptr+1
        .endmacro

	.macro	PointerTestAndCall ptr
	PointerTestZero ptr
	beq	*+11		;2
	jsr	*+6		;3
	jmp	*+6		;3
	jmp	(ptr)		;3
	.endmacro

	.macro	IncrementAndLoad ptr
	.local	@skip
	inc	0+(ptr)
	bne	@skip
	inc	1+(ptr)
	@skip:
	lda	(ptr),y
	.endmacro

	.macro	IncrementAndYLoad ptr
	ldy	#0
	IncrementAndLoad ptr
	.endmacro


;;; Increment a pointer and load a value, Y register is leaved unchanged.
;;; Input: ptr1
;;; Modifies: A, ptr1
;;; Output: ptr1=increment by 1, A=byte pointed to by ptr1
	.global	increment_and_load_ptr1


;;; Increment a pointer and load a value, Y is set to zero.
;;; Input: ptr1
;;; Modifies: A, Y, ptr1
;;; Output: ptr1=increment by 1, A=byte pointed to by ptr1, Y=0
	.global increment_and_y_load_ptr1
