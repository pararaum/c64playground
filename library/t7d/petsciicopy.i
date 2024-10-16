;;; -*- mode: asm -*-
;;; Routine to copy an uncompressed petscii frame to the screen.

;;; Address of the screen memory this functions will use.
	.global	PETSCIICOPY_SCREEN

;;; Copy a petscii frame.
;;; Input: AX=pointer to the petscii frame
;;;	PETSCIICOPY_SCREEN=address of the screen memory to copy to, $400 by default
;;; Output: Y=1
;;; Modifies: A,X,Y,ptr1
	.global	petsciicopyframe
