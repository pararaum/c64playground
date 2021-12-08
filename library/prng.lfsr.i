;; -*- mode: asm -*-

;;; Seed the LFSR random number generator.
;;; Input: A/X = 16 Bit seed, must not be zero (LO/HI)
;;; Output: -
;;; Modifies: A
	.import	lfsr16_seed

;;; Call the LFSR random number generator.
;;; Input: -
;;; Output: A/X = random value (LO/HI)
;;; Modifies: A, X
	.import	lfsr16_call

;;; Set the new feedback term.
;;; Input: A/X = 16bit feedback term.
;;; Output: -
;;; Modifies: -
	.import	lfsr16_set_feedbackterm
