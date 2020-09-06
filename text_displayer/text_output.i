; -*- mode: asm -*-
	.import	tedi_chrout
	.import	tedi_init
	.import	tedi_output_text
	.import tedi_switch_charset

;;; Macro to call the text output function.
;;; txtaddr: address of the text to be outputted
;;; stdchar: 0=custom charset, ¬0=default charset
	.macro	call_tedi_out	txtaddr,stdchar
	 .if stdchar = 0
	  sec
	 .else
	  clc
	 .endif
	 jsr	tedi_switch_charset
	 lda	#<txtaddr
	 ldx	#>txtaddr
	 jsr	tedi_output_text
	 jsr	wfk
	.endmacro

;;; Advanced macro to use the tedi output function. It will switch to the DATA segment, load the date from the file name, then switch to the code segment and use the call_tedi_out macro.
;;; fname: file name to include
;;; stdchar: see call_tedi_out
	.macro	tedi_inc_n_call fname,stdchar
	.local	DATA
	.data
DATA:	.incbin	fname
	.byte	0
	.code
	call_tedi_out	DATA,stdchar
	.endmacro


