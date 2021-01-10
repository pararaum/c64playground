
;;; Get the next value from the PRNG.
;;; Input: -
;;; Output: A/X = 16 bit random number, low in A
;;; Modifies: A, X
	.import	xorshift

;;; Initialise the PRNG
;;; Input: A/X = seed value, must be different from zero!
;;; Output: -
;;; Modifies: -
	.import	xorshift_seed
