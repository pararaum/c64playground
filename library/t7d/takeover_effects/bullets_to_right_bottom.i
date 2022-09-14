;;; -*-mode: asm -*-

;;; Init the takeover effect engine. *Must* be called first!
;;; Input: A=colour to use for the character foreground
;;;	CHARGEN_DESTINATION: Address where the font will be copied
;;; Modifies: A, X
;;; Output: -
	.import	init_takeover_bulleteffect

;;; The update function must be called once per frame.
;;; Input: -
;;;	__SCREEN0_START__: Address of the text screen ($400)
;;; Modifies: A, Y
;;; Output: C=0: finished the effect
	.import	update_takeover_bulleteffect
