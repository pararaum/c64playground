; -*- mode: asm -*-

;;; Change characters periodically.

	;; A characterset to use for wiggling.
;;; 	.global	WIGGLE_CHARSET
	.global	WIGGLE_SOURCE_LO
	.global	WIGGLE_SOURCE_HI


	;; A characterset into which the wiggle characters are copied.
	.global	WIGGLE_DESTINATION_CHARSET

	;; List of characters to be wiggled.
	.global	WIGGLE_CHARS

;;; Call once per frame to update the wiggle characters.
;;; Input: A=number of wiggle entries
;;; Modifies: *
	.global	update_wiggle
