; -*- mode:asm -*-

;;; Multiply Y register times 40 and store the result in ptr1.
;;; Input: Y = line to use
;;; Output: ptr1 = offset from the screen base, A = LO(ptr1)
;;; Modifies: A
	.import	y_times_40
