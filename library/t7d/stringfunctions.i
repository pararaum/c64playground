; -*- mode: asm -*-

;;; Macro to generate text which is shifted one bit to the left and the LSB is set to 1 if this was the last character of the string.
;;; Input: arg=string to be embedded as byte sequence.
;;; Output, Modifies: -
	.macro ShiftedLeftText arg
	 .repeat	.strlen(arg)-1,I
	  .byte		<(.strat(arg, I)<<1)
	 .endrepeat
	 .byte		<((.strat(arg, .strlen(arg)-1)<<1)|1)
	.endmacro


;;; Macro to generate text which has the MSB is set to 1 if this was the last character of the string. This is a method used in the BASIC ROM.
;;; Input: arg=string to be embedded as byte sequence.
;;; Output, Modifies: -
	.macro ShiftedText arg
	 .repeat	.strlen(arg)-1,I
	  .byte		.strat(arg, I)
	 .endrepeat
	 .byte		<((.strat(arg, .strlen(arg)-1))|$80)
	.endmacro


;;; Output a string using CHROUT, string may be longer than 255 Bytes.
;;; Input: A/X=address to the string
;;; Modifies: A
;;; Output: -
	.import	output_long_string



;;; Output a string usr CHROUT, deluxe variant.
;;;	Control characters are just passed to CHROUT, but Return ($0D)
;	moves the cursor to the next line within the preselected
;	window. Warning! Clear screen $93 will clear the whole screen!
;;; 
;;; An special code is the PETSCII code $01, other wise unused, this
;;; will reread the window parameters in the next four bytes.
;;; 
;;; Input: Parameters are passed via program memory (bytes after the JSR)
;;;	- X-position
;;;	- Y-position
;;;	- width
;;;	- height
;;; 	- text, ends with a null byte ($0)
;;; Modifies: A, X, Y, ptr1
;;; Output: -
	.import output_string_deluxe
