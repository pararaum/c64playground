;;; -*- mode:asm -*-

;;; This routine decompress ZX02 streams, see here: https://github.com/dmsc/zx02/tree/main
;;; As zeropage addresses ptr1, ptr2, ptr3, ptr4, and tmp1 are used. ptr1 and ptr2 must be in consecutive memory positions.

;;; Decompress ZX02 compressed data.
;;; Input:
;;;	- ptr1=pointer to the compressed source data
;;;	- ptr2=pointer to the destination memory
;;; Output: (Y=0?)
;;; Modifies: A, X, Y, ptr1, ptr2, ptr3, ptr4, tmp1
	.import zx02decrunch

;;; Decompress ZX02 compressed data, parameter in program memory.
;;; Input: 2 words after the JSR
;;;	- pointer to the compressed source data
;;;	- pointer to the destination memory
;;; Output: (Y=0?)
;;; Modifies: A, X, Y, ptr1, ptr2, ptr3, ptr4, tmp1
	.import zx02decrunch_ip
