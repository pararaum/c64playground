	.include	"t7d/scroller/fsvscroll.i"

.proc	_fsvscroll_update_softscroll_up_1
	jsr	fsvscroll_softscroll_up_1
	ora	#%00010000	; Screen on, 24 lines, scroll.
	sta	$d011
	lda	fsvscroll_softscroll_val ; Get scroll position value.
	cmp	#7
	beq	do_copy_up
	cmp	#0
	beq	copy_to_spare
	rts
do_copy_up:
	lda	$d018		; Get old vic screen address.
	eor	fsvscroll_d018_pageflip_eor
	sta	$d018		; And now flip.
	;; Copy colour codes.
	lda	#$d8		; Setup page for screen copy.
	tax
	jsr	fsvscroll_copy_line_up
	jmp	(fsvscroll_copy_next_line_fun)
copy_to_spare:
	FSVScroll_SwitchScreenSpare
	jmp	fsvscroll_copy_line_up
.endproc
