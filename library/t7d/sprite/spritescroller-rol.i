;;; -*- mode: asm -*-

;;; Spritescroller using eight sprites and the characters are moved along with ROL.

        ;; The colour of the sprite scroller, will be copied to the sprite registers.
        .global SPRITESCROLLER_COLOUR

	;; Eight consecutive sprite buffers. These sprite buffers are used for the scroller. 8*64=512 Bytes.
	.global	SPRITESCROLLER_ROL_SPRITEBUFFER

	;; Address of the character generator aka font for the scroller.
	.global SPRITESCROLLER_ROL_CHARGEN

;;; Initialise the horizontal sprite scroller.
;;;
;;; Basic initialisation of sprite scroller. This function will clear the whole spritescroller rol spritebuffer with zeroes.
;;;
;;; Input: AX=pointer to the text, Y=Y-position of the scroller
;;; Output: -
;;; Modifies: A, X
        .global spritescroller_rol_init

;;; Initialise the sprite registers with position, colour, turn sprites on and turn on double width, sprite pointers are *not* set.
;;;
;;; Input: -
;;; Output: -
;;; Modifies: A, X, Y
	.global spritescroller_rol_init_sprite_registers

;;; Call once per frame to update the scroller.
;;;
;;; This function only accesses the spritebuffer memory and the character generator, no VIC addresses are used. Therefore if the build-in font is going to be used set the memory configuration to a visible character ROM and set SPRITESCROLLER_ROL_CHARGEN to either $D000 or $D800.
;;;
;;; Input: -
;;; Output: -
;;; Modifies: A, X, Y 
        .global spritescroller_rol_update
