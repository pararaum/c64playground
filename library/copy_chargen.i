
; -*- mode: asm -*-

;;; Copy the character generator ROM to CHARGEN_DESTINATION
;;;
;;; The memory configuration will be set to chargen ROM and restored afterwards.
;;; Warning! Disable interrupts before calling this routine!
;;;
;;; Import: -
;;;	CHARGEN_DESTINATION: destination address
;;; Modifies: A, X
;;; Output: -
	.import	copy_chargen
