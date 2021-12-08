; -*- mode: asm -*-
;;; Routines to print text into sprite buffers. It is assumed that the sprite buffers are consecutive in memory (and that there are eight of them).

	;; Pointer to the first of the consecutive sprite buffers.
	.importzp	SPRITETEXT_DESTINATION

	;; Print a character into a sprite buffer.
	;; Input: A/X=pointer to the target, Y=character, SPRITETEXT_CHARGEN
	;; Output: -
	;; Modifies: A, X, Y
	.import print_char_to_sprite

	;; Print a whole line (8 * three chars) into consecutive sprite buffers.
	;; Input: A/X=pointer to the text (24 characters), SPRITETEXT_DESTINATION
	;; Output: -
	;; Modifies: A, X, Y, ptr1, SPRITETEXT_DESTINATION
	.import	print_line_to_sprite
