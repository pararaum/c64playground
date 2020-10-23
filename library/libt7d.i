; -*- mode: asm -*-

        .import _disable_cia_irq

;;; Fill the colour RAM
;;; Input: A=colour value
;;; Output: -
;;; Modifies: X
	.import	_fill_colour_ram
