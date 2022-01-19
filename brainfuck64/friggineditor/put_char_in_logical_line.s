
	.importzp	sp		; C stack pointer.
	.import	popa

	.export	_put_char_in_logical_line

;;; void __fastcall__ put_char_in_logical_line(unsigned char pos, char ch);
;;; ch in A/X (fastcall)
;;; pos on C stack
_put_char_in_logical_line:
	pha			; Store character on stack.
	ldy	#0
	lda	(sp),y		; Get position.
	tay			; And move to index register.
	pla			; Get character back from stack.
	bmi	no_conversion
	cmp	#64		; Less than 64?
	bcc	no_conversion
	cmp	#96
	bcc	sub64
	sec
	sbc	#32
no_conversion:
	sta	($d1),y
	jmp	end
sub64:	sec
	sbc	#64
	sta	($d1),y
end:	jmp	popa		; Remove element from C stack (cleanup).
