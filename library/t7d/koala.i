;;; -*- mode: asm -*-

;;; Convenience functions for handling Koala Painter pictures.

;;; Copy Koala picture image-data to destination area.
;;; Input: pointers are expected behind the JSR.
;;; 	- source pointer (koala picture without load address)
;;; 	- destination bitmap data
;;; 	- destination chars (colour information)
;;; 	- destination colours (for colour RAM use $d800)
;;; Output: -
;;; Modifies: A, X, Y, ptr1, ptr2, ptr3
	.import copy_koala_picture_data
