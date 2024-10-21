; -*- mode: asm -*-
;;; A routine to creat a canvas using sprites.

;;; Initialise the canvas engine for 3 sprites horizontally and four
;	sprites vertically. The parameters are passed in the code just
;	behind the JSR.
;;; Input: Parameters after the JSR:
;;;	.word	x-position of the canvas
;;; 	.byte	y-position of the canvas
;;; 	.byte	number of the sprite buffer of the top-left sprite
;;; 	.word	address of the sprite pointer for the 0-th sprite, $400+1024-8 in the default configuration
;;; 	.byte	colour of the sprite canvas
;;; 	.byte	delay time before the sprite parameters are written to the VIC, adjust this if you see a flicker, 8 cycles should be fine if the sprite canvas start in the left part of the screen
;;; Modifies: A, X, Y, ptr1
;;; Output: -
	.global	spritecanvas3X4_init

;;; Routine to be called at each interrupt, this routine must be called at least ... rasterlines before the top position of the sprite canvas.
;;; Input: -
;;; Modifies: *
;;; Output: -
	.global	spritecanvas3X4_irq
