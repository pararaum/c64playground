; -*- mode: asm -*-


;;; Clear the time of day clock (and set it to 50 Hz).
;;; Input: -
;;; Modifies: A
;;; Output: -
	.global	clear_cia1_tod
