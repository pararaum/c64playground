;;; -*- mode: asm -*-
;;; Engine for Sprite movement where the X-position ist stored
;;; 	halfed. See
;;;	https: //codebase64.pokefinder.org/doku.php?id=base:moving_sprites.

;;; In this engine the sprite position is stored half but we need
;	still four bytes for each sprite. This is the x- and the
;	y-positions and the x- and y-velocity. Sprites are moved by
;	simple vector addition.

	;; Pointer to the currently used work area.
	.globalzp SPRX2E_POINTER_CURRENT_WORKAREA

;;; Init the engine.
;;; Input: A/X=pointer to the work area, Y=0 clear the area, 1=keep old values.
;;; Output: Y=$FF
;;; Modifies: A, Y
	.import	sprX2E_init

;;; ######################  All functions below must be called after init!

;;; Copy the sprite shadow registers to the VIC registers.
;;; Input: -
;;; Output: -
;;; Modifies: A, X, Y
	.global	sprX2E_copy_sprite_shadowregs

;;; Update the sprite positions with the known velocities.
;;; Input: -
;;; Output: -
;;; Modifies: A,X
	.global sprX2E_update_sprites
