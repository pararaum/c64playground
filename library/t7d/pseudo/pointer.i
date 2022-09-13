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
