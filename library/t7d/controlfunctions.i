;;; -*- mode: asm -*-

;;; Run a list of function pointers, each entry is JSRed to and if the Carry flag is set the entry index is incremented otherwise on the next call the same index is used.
;;; Usage example:
;	.rodata
;pointerlist:
;	.word	func1		; First function to call.
;	.word	func2		; second
;	.word	func3		; third, etc. ...
;pointerlist_end:
;
;	.code
;	;; ...
;	jsr	run_function_list
;	brk			; actually ".byte 0"
;	.byte	(pointerlist_end-pointerlist)/2 ; length of list
;	.word	pointerlist	; list of pointers to JSR to
;	;; code continues here
;;; Input: 4 bytes after the JSR
;;; Output: Z=1 if maximum list entry reached
;;; Modifies: A, X, Y, ptr1, ptr2, byte after JSR
	.global	run_function_list
