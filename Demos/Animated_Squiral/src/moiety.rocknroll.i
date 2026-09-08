; -*- mode: asm -*-

;;; The destination textscreen where the current area is copied to. There are two screens as we use double buffering.
	.global	ROCKNROLL_TEXTSCREEN0
	.global	ROCKNROLL_TEXTSCREEN1

;;; The X and Y position of the screen window (in pixeln) for the draw. These variables are updated in update_rocknroll and used in draw_rocknroll
	.global	ROCKNROLL_XOFFSET
	.global	ROCKNROLL_YOFFSET
	.global	ROCKNROLL_XCELL
	.global	ROCKNROLL_YCELL


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
;;;	- pointer to the source screen
;;;	X=source width, Y=source height (max 50!)
;;; Modifies: A,X,Y
;;; Output: -
	.global	init_rocknroll


;;; Set the initial pointer of X-positions which will be reloaded the next time the end is reached. This does *not* change the current pointer!
;;; Input: A/X=pointer to new x-positions
;;; Modifies: -
;;; Output: -
	.global	reinit_rocknroll_Xpos

;;; Set the initial pointer of X-positions which will be reloaded the next time the end is reached. This does *not* change the current pointer!
;;; Input: A/X=pointer to new x-positions
;;; Modifies: -
;;; Output: -
	.global	reinit_rocknroll_Ypos

;;; Draw the rocknroll screen. This will change horizontal and vertical scroll register, switch between the text screens and start to copy the data for the next screen. This take quite a bit of time.
;;; Input: ROCKNROLL_XOFFSET,ROCKNROLL_YOFFSET
;;; Modifies: A,X,Y,ptr1,tmp1
;;; Output: -
	.global	draw_rocknroll

;;; Get the next x/y-coordinates from the list and prepare everything for the drawing of the next frame.
;;; Input: -
;;; Modifies: A,X,Y,ptr1,tmp1
;;; Output: ROCKNROLL_XOFFSET,ROCKNROLL_YOFFSET
	.global	update_rocknroll

;;; Change the rock'n'roll source screen pointer.
;;; Input: AX=new screen pointer
;;; Modifies: A,X
;;; Output: -
	.global	change_rocknroll

;;; Change the rock'n'roll x-position pointer. For delayed action see reinit_rocknroll_?pos above.
;;; Input: AX=new x-position pointer
;;; Modifies: -
;;; Output: -
	.global	change_rocknroll_xpos

;;; Change the rock'n'roll y-position pointer. For delayed action see reinit_rocknroll_?pos above.
;;; Input: AX=new y-position pointer
;;; Modifies: -
;;; Output: -
	.global	change_rocknroll_ypos
