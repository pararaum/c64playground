	.include	"t7d/pseudo/pointer.i"
	.include	"t7d/kernal.i"
	.include	"t7d/stackmacros.i"
	.macpack	longbranch

	.importzp	ptr1

	.export		output_string_deluxe
;;; An inspiration for this routine can be found in the Timex source, see https://csdb.dk/release/?id=58460.

	.bss
top:	.res	1
left:	.res	1
right:	.res	1
bottom:	.res	1
char:	.res	1

	.code
output_string_deluxe:
	GetReturnAddrIntoPointer	ptr1
resize_window:
	jsr	increment_and_y_load_ptr1 ; Left
	sta	left
	jsr	increment_and_load_ptr1 ; Top
	sta	top
	jsr	increment_and_load_ptr1 ; Width
	clc
	adc	left
	sta	right
	jsr	increment_and_load_ptr1 ; Height
	clc
	adc	top
	sta	bottom
	dec	bottom
restart_current_window:
	ldx	top
	ldy	left
	clc
	jsr	PLOT
	;; Cursor was set, now output.
printloop:
noprint:
	jsr	increment_and_y_load_ptr1 ; Next char
	jeq	fini
	cmp	#1		; Special code, resize the window parameters.
	beq	resize_window
	cmp	#$D		; Return
	beq	print_return
	cmp	#$93		; Clear
	beq	print_clear
	cmp	#$13		; Home
	beq	restart_current_window
	sta	char
	lda	$d3		; Cursor column
	cmp	right
	bcs	noprint
	lda	$d6		; Cursor row
	cmp	top
	beq	okprint
	bcc	noprint		; Above the window.
	cmp	bottom
	beq	okprint		; Bottom line of the window.
	bcs	restart_current_window		; Below the window.
okprint:
	lda	char
	jsr	CHROUT
	jmp	printloop
print_return:
	ldx	$d6		; Get row.
	ldy	left		; Column to the left!
	inx
	cpx	bottom
	beq	equal1
	bcs	restart_current_window
equal1:	clc
	jsr	PLOT
	jmp	printloop
print_clear:
	jsr	CHROUT
	jmp	restart_current_window
	;; Last byte was read, it is a zero byte, the pointer is still pointing at the zero-byte. Neat for us that this corresponds to returnaddr-1 for the CPU!
fini:
	PointerToStack	ptr1
	rts
