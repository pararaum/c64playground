;;; xa -M -I ../include/ -l /dev/stdout usr_with_string.asm

;;; A simple USR() function. If it called with a numeric expression then the value is returned. If it is called with a string expression the string is printed followed by the number of characters the string has.

#include "c64basic.inc"
#include "c64kernal.inc"

	.word	$C000
	* = $c000

	jmp	enableUSR
	jmp	disableUSR

usraddsave:
	.word	0

enableUSR:
	lda	USRADD
	sta	usraddsave
	lda	USRADD+1
	STA	usraddsave+1
	lda	#<usrcode
	sta	USRADD
	lda	#>usrcode
	sta	USRADD+1
	lda	#2
	sta	$d020
	rts

disableUSR:
	lda	usraddsave
	sta	USRADD
	lda	usraddsave+1
	sta	USRADD+1
	rts

usrcode: .(
	bit	VALTYP		; Value is string or numeric?
	bmi	_string
	;; Do nothing if numeric.
	rts
_string:
;;; D. Heeb, "Compute!'s VIC-20 and C64 Tool Kit: BASIC", 1984.
;;; The book says on page 135 that a string expression stores a pointer in $64 to the string descriptor of the temporary string.
	ldy	#0
	lda	($64),y		; Get length of string.
	pha			; Push on stack.
	sta	_outSTRLEN
	ldy	#1
	lda	($64),y		; Get LOW of string.
	sta	_outPTR
	iny
	lda	($64),y		; Get HIGH of string.
	sta	_outPTR+1
	ldx	#0
_l1:	lda	$ffff,x
	_outPTR=*-2		; For self-modifiying code.
	jsr	CHROUT
	inx
	cpx	#$ff
	_outSTRLEN=*-1		; Self-modifying code.
	bne	_l1
	;; https://www.c64-wiki.com/wiki/USR says to do the following.
	jsr	FREFAC		; Free the string on the stack.
	lda	#0
	sta	VALTYP		; VAL is now numeric.
	pla			; A=length of string.
	;; L. English, "The Advanced Machine Language Book", p 22.
	jsr	$BC3C		; Convert A into signed float.
	rts
	.)

	;; EOF
