; -*- mode: asm -*-

;;; The destination textscreen where the current area is copied to. There are two screens as we use double buffering.
	.global	ROCKNROLL_TEXTSCREEN0
	.global	ROCKNROLL_TEXTSCREEN1

;;; Initialise the rocknroll engine
;;; 
;;; The positions are 16-bit words which define how many pixel the
;;; "window" should be offset in the source screen (so 8 pixel in
;;; width/height for a whole character). Valid value are from
;;; [0..32767], any value with bit 7 set in the high byte will reset
;;; the pointer of the corresponding coordinate. The lists of x- and
;;; y-positions do not need to be of same length.
;;; 
;;; Input: via Stack, push in order
;;;	- pointer to x-positions (LO, HI)
;;;	- pointer to y-positions (LO, HI)
;;;	- pointer to the source screen (width 64 chars, any height)
;;; Modifies: A,X,Y
;;; Output: -
	.global	init_rocknroll

;;; Draw the rochnroll screen. This will change horizontal and vertical scroll register, switch between the text screens and start to copy the data for the next screen. This take quite a bit of time.
;;; Input: -
;;; Modifies: A,X,Y,ptr1,tmp1
;;; Output: -
	.global	draw_rocknroll

;;; Get the next x/y-coordinates from the list and prepare everything for the drawing of the next frame.
;;; Input: -
;;; Modifies: A,X,Y,ptr1,tmp1
;;; Output: -
	.global	update_rocknroll

;;; Change the rock'n'roll source screen pointer.
;;; Input: AX=new screen pointer
;;; Modifies: -
;;; Output: -
	.global	change_rocknroll

;;; Change the rock'n'roll x-position pointer.
;;; Input: AX=new x-position pointer
;;; Modifies: -
;;; Output: -
	.global	change_rocknroll_xpos

;;; Change the rock'n'roll y-position pointer.
;;; Input: AX=new y-position pointer
;;; Modifies: -
;;; Output: -
	.global	change_rocknroll_ypos
