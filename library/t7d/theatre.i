; -*- mode: asm -*-

	.include	"t7d/theatre/consts.i"

;;; Constants for configuring the act initialisation
THEATRE_COPY_CHARGEN = $1
THEATRE_BITMAP_MODE = $2
THEATRE_MULTICOL_MODE = $4
THEATRE_COPY_FONT0800 = $8
	;; Copy background to the colour RAM otherwise copy area from THEATRE_COLR. If set the screen is filled with blanks.
THEATRE_ACT_BLANK = $16

;;; Framecounter, incremented once each frame (16-bit).
	.global	_theatre_frame_counter

;;; Wait until next frame. This variable is incremented in the IRQ.
	.global	_theatre_wait_frame

;;; Animate a sprite using the normal SpriteAnimationEntry.
;;; Input: X=sprite number, ptr1=pointer to the sprite data or NULL
;;; Output: -
;;; Modifies: A, Y, ptr1
	.global	theatre_animate_sprt

;;; Animate all eigth sprites
;;; Input: ptr1=pointer to the animation structure
;;; Modifies: A, X, Y, ptr1
	.global	theatre_animate_8

;;; Universal data pointer
;;; Used for various operations.
	.globalzp	_theatre_universal_pointer

	.global	_theatre_copy_compressed_frame

;;; After the theatre interrupt-routine has finished this vector is used to call a user-defined interrupt service routine (if ≠ 0). Warning! Only ptr1, ptr2, tmp1, tmp2 are saved by the ISR, so beware!
	.global	_theatre_irq_isr_vector

	.struct TheatreSceneConfiguration
	isr	.word
	interpicture	.word
	.endstruct
