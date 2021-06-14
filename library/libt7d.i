; -*- mode: asm -*-

;;; Disable the CIA interrupts.
        .import _disable_cia_irq

;;; Set the kernal interrupt pointer.
;;; Input: addr=address to the kernal pointer to
;;; Modifies: A
;;; Output: -
.macro	SeqIRQ314Pointer	addr
	lda	#<addr
	sta	$314
	lda	#>addr
	sta	$315
.endmacro

;;; Fill the colour RAM
;;; Input: A=colour value
;;; Output: -
;;; Modifies: X
	.import	_fill_colour_ram

