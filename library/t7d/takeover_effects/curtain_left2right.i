;;; -*-mode: asm -*-

;;; Pointer to the C64 screen which is to be dissolved.
;;; This is probably $0400 but may be changed if needed.
	.global	TAKEOVER_SCREENBASE

;;; Pointer to a sprite buffer to be used, only 64 bytes are needed.
	.global	TAKEOVER_SPRITEBUFFER

;;; Global variable that can be tested if the effect has finished. If
;	it zero then the effect has finished.
	.global	takeover_curtain_left2righ

;;; Init the takeover effect engine. *Must* be called first!
;;; Input: A=colour to use for the character foreground
;;;	TAKEOVER_SCREENBASE: Address where the C64 screen is to be found.
;;;	TAKEOVER_SPRITEBUFFER: 64 Bytes will be filled.
;;; Modifies: A, X
;;; Output: -
	.import	init_takeover_curtain_left2right

;;; The update function must be called once per frame.
;;; Input: -
;;;	TAKEOVER_SCREENBASE: Address of the text screen ($400)
;;; Modifies: A, Y, ptr1, tmp1
;;; Output: takeover_dissolve_left2righ=0: finished the effect
	.import	update_takeover_curtain_left2right
