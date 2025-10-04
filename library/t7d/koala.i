;;; -*- mode: asm -*-

;;; Convenience functions for handling Koala Painter pictures.

;;; Copy Koala picture image-data to destination area. Warning! Background colour is not changed!
;;; Input: pointers are expected behind the JSR.
;;; 	- source pointer (koala picture without load address)
;;; 	- destination bitmap data
;;; 	- destination chars (colour information)
;;; 	- destination colours (for colour RAM use $d800)
;;; Output: -
;;; Modifies: A, X, Y, ptr1, ptr2, ptr3
	.import copy_koala_picture_data

;;; Copy Koala picture image-data to destination area. Also set background colour (expected after bitmap and colour data).
;;; Input: pointers are expected behind the JSR.
;;; 	- source pointer (koala picture without load address)
;;; 	- destination bitmap data
;;; 	- destination chars (colour information)
;;; 	- destination colours (for colour RAM use $d800)
;;; Output: -
;;; Modifies: A, X, Y, ptr1, ptr2, ptr3
	.import copy_koala_picture_data_bg
