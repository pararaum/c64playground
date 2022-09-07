	.include	"t7d/scroller/fsvscroll.i"

	.macpack	generic

	DUMMY_SCREEN_POINTER := $400

	.bss
fsvscroll_current_screenpage:
	.res	1
fsvscroll_spare_screenpage:
	.res	1

	.code
.proc	fsvscroll_draw_next_line_at_bottom
	lda	fsvscroll_current_screenpage	; Page of the current screen.
	add	#3		; Last line (1000-40=$3c0).
	sta	destptr+1
	ldy	#40-1
l1:	lda	(fsvscroll_character_ptr),y
	sta	DUMMY_SCREEN_POINTER+1000-40,y
	destptr=*-2
	lda	(fsvscroll_colour_ptr),y
	sta	$D800+1000-40,y
	dey
	bpl	l1
	P_addimm	40,fsvscroll_character_ptr
	P_addimm	40,fsvscroll_colour_ptr
	P_cmp	fsvscroll_character_end,fsvscroll_character_ptr
	rts
.endproc
