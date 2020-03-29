;;; xa -M -I ../include/ -l /dev/stdout basic_wedge.asm
;;;
;;; vice: x64 -remotemonitor
;;; reload: echo 'bl "a.o65" 0 c000' | nc -N localhost 6510

;;; A simple BASIC wedge which will add an additional command using
;	the basic wedge technique with an "excape" character as
;	described here:
;
;	 * C64 Intern
;	 * https://codebase64.org/doku.php?id=base:basicwedge
;	 * Jim Butterfield, Machine Language for the C64, 128, and other Commodore Computers, Revised & Extended Edition, p. 124.
;
;	This is not the same as extending the Basic properly with new
;	tokens as described in this book:
;
;	 * Lothar Englisch, The Advanced Machine Language Book for the Commodore-C64, p. 160.
;	

#include "c64basic.inc"
#include "c64kernal.inc"

	Page2Free = $02a7	; $2a7 to $2ff is free.
	igonesave = Page2Free	; PTR to save IGONE
	
	* = $c000

	jmp	enablewedge
	jmp	disablewedge
	jmp	*
	jmp	*
	jmp	*
	jmp	*
	jmp	*
wedgevector:
	jmp	wedgecode
	
enablewedge:
	lda	IGONE
	sta	igonesave
	lda	IGONE+1
	STA	igonesave+1
	lda	#<wedgevector
	sta	IGONE
	lda	#>wedgevector
	sta	IGONE+1
	lda	#2
	sta	$d020
	rts

disablewedge:
	lda	igonesave
	sta	IGONE
	lda	igonesave+1
	sta	IGONE+1
	rts

wedgecode: .(
	jsr	CHRGET
	cmp	#'_'		; Is the character the arrow to the left?
	beq	_found		; If yes jump to processing.
	jsr	CHRGOT		; Restore flags.
	JMP	GONE+3		; Continue Basic.
_found:
	jsr	CHRGET		; Get next character.
	cmp	#'A'		; Is this an A?
	beq	_command
	cmp	#$99		; PRINT
	beq	_printcmd
	ldx	#17		; Undefined statement error. See https://sta.c64.org/cbm64baserr.html.
	jmp	(IERROR)
_command:
	lda	#$93
	jsr	CHROUT
	jmp	(IGONE)
_printcmd:
	lda	#'*'
	ldx	#40
_l75:	jsr	CHROUT
	dex
	bne	_l75
	jmp	(IGONE)
	.)


	;; EOF
