;;; -*- mode: asm -*-

;;; Simple routine to perform a two frame animation with different speeds.
;;; The routine needs a working area (point A/X to it) with 24 bytes:
;;; 	- current frame counters (8 bytes)
;;; 	- number of frames-1 to skip before animation (8 bytes, one per frame)
;;; 	- value to XOR the sprite pointer with
;;; Input: A/X=pointer to animation XOR info.
;;;	ptr1=pointer to the sprite pointers.
;;; Output: -
;;; Modifies: A, Y, ptr2
	.import	animspriteXORptr
