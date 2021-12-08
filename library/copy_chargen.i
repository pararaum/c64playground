
; -*- mode: asm -*-

;;; Copy the character generator ROM to CHARGEN_DESTINATION
;;; Import: -
;;;	CHARGEN_DESTINATION: destination address
;;; Modifies: A, X
;;; Output: -
	.import	copy_chargen
