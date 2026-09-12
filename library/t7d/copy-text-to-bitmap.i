; -*- mode: asm -*-

;;; This routine will copy the textscreen using the character
;	generator font information into a bitmap. This can be nicely
;	used for takeover effects.

;;; Input: ptr1=textscreen, ptr2=chargen, ptr3=bitmapdestination
;;; Output: -
;;; Modifies: A,X,Y,ptr1,ptr2,ptr3,ptr4
	.import	copy_text_to_bitmap

