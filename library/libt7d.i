; -*- mode: asm -*-

;;; Disable the CIA interrupts.
        .import _disable_cia_irq

;;; Set the kernal interrupt pointer.
;;; Input: addr=address to the kernal pointer to
;;; Modifies: A
;;; Output: -
.macro	SetIRQ314Pointer	addr
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

;;; Fill exactly 1000 bytes with a value (e.g. screen).
;;; Input: A/X=pointer to memory, Y=value to fill with
;;; Output:
;;; Modifies: A,X,Y,ptr1
	.import	fill_1000_bytes
