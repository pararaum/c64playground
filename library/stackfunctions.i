;;; -*- mode: asm -*-

;;; Adjust the stack pointer via A.
;;; Input: A=delta to add onto the stack pointer
;;; Output: -
;;; Modifies: A, X
	.import	adjust_stackptr
