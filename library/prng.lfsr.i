;; -*- mode: asm -*-

;;; Seed the LFSR random number generator.
;;; Input: A/X = 16 Bit seed, must not be zero (LO/HI)
;;; Modifies: A
;;; Output: -
	.import	lfsr16_seed

;;; Call the LFSR random number generator.
;;; Input: -
;;; Modifies: A, X
;;; Output: A/X = random value (LO/HI)
	.import	lfsr16_call
