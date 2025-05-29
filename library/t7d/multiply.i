; -*- mode: asm -*-
	.include	"t7d/multiply_by_40.i"

;;; Multiply A register times 64 and store the result in ptr1.
;;; Input: A = vlaue to multiply
;;; Output: ptr1 = A*64
;;; Modifies: A
        .import a_times_64
	.import	x_times_64
	.import	y_times_64
