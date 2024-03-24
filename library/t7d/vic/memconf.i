; -*- mode: asm -*-

;;; Set the VIC screen memory in the current VIC bank
;;; Input: A=HI of screen memory address
;;; Output: A=new value of $d018
;;; Modifies: tmp1
	.import	set_screen_memory

;;; Set the VIC character generator address in the current VIC bank
;;; Input: A=HI of chargen memory address
;;; Output: A=new value of $d018
;;; Modifies: tmp1
	.import	set_chargen_memory
