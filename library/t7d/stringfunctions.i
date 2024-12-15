; -*- mode: asm -*-

;;; Output a string using CHROUT, string may be longer than 255 Bytes.
;;; Input: A/X=address to the string
;;; Modifies: A
;;; Output: -
	.import	output_long_string
