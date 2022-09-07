
	.macpack	generic

	.export fsvscroll_copy_line_up

	;; This is only a dummy! Used to have a defined screen address.
	DUMMY_SCREEN_POINTER := $400

.proc	fsvscroll_copy_line_up
	sta	sptr+1
	stx	dptr+1
	;; We will only add two pages not three as we are using the "shift trick", see below.
	add	#2		; Copy remaining pages pointer.
	sta	sptr2+1
	txa
	add	#2
	sta	dptr2+1
	ldy	#3-1		; Copy three times 256 chars.
l4:	ldx	#0		; Copy 256 chars.
l2:
	lda	DUMMY_SCREEN_POINTER+40,x
	sptr=*-2
	sta	DUMMY_SCREEN_POINTER+00,x
	dptr=*-2
	inx
	bne	l2
	inc	sptr+1
	inc	dptr+1
	dey
	bpl	l4
	;; (- 1000 (* 3 256))232
	;; (- 1000 (* 3 256) 40)192
	remaining_chars = 1000-3*256-40 ; $C0
	;; Trick not to have to check if the limit was reached, start with a value shifted.
	shift_value = 256-remaining_chars
	ldx	#shift_value
l5:	lda	DUMMY_SCREEN_POINTER+40+2*256-shift_value,x
	sptr2=*-2
	sta	DUMMY_SCREEN_POINTER+0+2*256-shift_value,x
	dptr2=*-2
	inx
	bne	l5
	rts
.endproc

