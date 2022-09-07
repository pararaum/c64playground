;;; Function to initialise the data as in the FSVScroll_Initialise Macro.
;;; See fsvscroll.i

	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/scroller/fsvscroll.i"
	.include	"t7d/multiply_by_40.i"
	.importzp	ptr1

	.export	_fsvscroll_init

_fsvscroll_init:
	sta	ptr1		; Store pointer for indirect access.
	stx	ptr1+1
	;; Get screen page address.
	ldy	#3
	lda	(ptr1),y
	sta	fsvscroll_current_screenpage
	;; Get spare page adress.
	ldy	#5
	lda	(ptr1),y
	sta	fsvscroll_spare_screenpage
	;; Now calculate the flip value.
	eor	fsvscroll_current_screenpage
	;; ($XX00/$400)<<4 to put into $d018 bits 4-7.
	;; $XX00/$100 is already moved two bits to the left, so...
	asl			; ...move remaining two bits to the left.
	asl
	sta	fsvscroll_d018_pageflip_eor
	;; Now get the frame character and colour data.
	ldy	#0
	lda	(ptr1),y	; Get pointer to character frame data.
	sta	fsvscroll_character_addr
	iny
	lda	(ptr1),y
	sta	fsvscroll_character_addr+1
	;; Copy to character end pointer, we will later add the size to it.
	P_transfer	fsvscroll_character_addr, fsvscroll_character_end
	ldy	#6		; Get height.
	lda	(ptr1),y
	tay			; Number of lines in Y.
	jsr	y_times_40	; Result is in ptr1.
	P_add	ptr1, fsvscroll_character_end ; Add to end.
	P_transfer	fsvscroll_character_end, fsvscroll_colour_addr ; This is also the colour information.
	jsr	fsvscroll_reset_scroll_ptrs
	lda	#3
	sta	fsvscroll_softscroll_val
	rts
