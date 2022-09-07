;;; -*- mode: asm -*-
;;; Full Screen Vertical Scroll
;;;
;;; Routines for implementing a full screen vertical scroller, it can
;	be used to create vertical scrollers in full width with and
;	with constant colour RAM.

	.include	"t7d/pseudo/pseudo16.inc"

	;; Pointer to the next character line to be copied.
	.globalzp	fsvscroll_character_ptr
	;; Pointer to the next colour line to be copied.
	.globalzp	fsvscroll_colour_ptr
	;; These define the default values.
	;; Pointer to the first character line of the screen.
	.global	fsvscroll_character_addr
	;; Pointer to the first colour line of the screen.
	.global	fsvscroll_colour_addr
	;; End of the character lines, must point to the byte after the last line.
	.global	fsvscroll_character_end

	;; Function pointer to copy the next line into the bottom of the currently visible screen.
	.global	fsvscroll_copy_next_line_fun ; Warning! Set this before any calls to fsvscroll_update_softscroll_up_1!

	;; Soft scroll value (0..7).
	.global	fsvscroll_softscroll_val

	;; Hi-byte of the current screenpage. This is the page fsvscroll_draw_next_line_at_bottom draws the line on.
	.global fsvscroll_current_screenpage
	;; Hi-byte of the spare screenpage.
	.global fsvscroll_spare_screenpage
	;; Pageflip value to be EORed into $D018.
	.global	fsvscroll_d018_pageflip_eor

;;; Initialise the scrolling framework for scrolling upwards.
;;;	framedata: pointer to the petscii data with height lines, followed bei lines of colour information
;;;	height: number of lines in the graphics
;;;	screenaddr: address of the visible screen
;;;	spareaddr: address of the spare page, will be switched to, when scrolling up linewise
	.macro	FSVScroll_Initialise framedata, height, screenaddr, spareaddr
	P_loadi	framedata, fsvscroll_character_addr
	P_loadi	framedata+40*height, fsvscroll_character_end
	P_loadi	framedata+40*height, fsvscroll_colour_addr
	jsr	fsvscroll_reset_scroll_ptrs
	lda	#3
	sta	fsvscroll_softscroll_val
	lda	#>(screenaddr)
	sta	fsvscroll_current_screenpage
	lda	#>(spareaddr)
	sta	fsvscroll_spare_screenpage
	lda	#((screenaddr^spareaddr)/$400)<<4
	sta	fsvscroll_d018_pageflip_eor
	.endmacro

;;; Convenience function to do a full softscroll update changing the VIC registers and performing data copy, page flip, etc.
;;; Input: global variables
;;; Output: -
;;; Modifies: *
	.global	_fsvscroll_update_softscroll_up_1

;;; Soft scroll one line upwards, it will return the current scroll value to be written to $d011.
	.global	fsvscroll_softscroll_up_1

;;; Switch spare with current page.
;;; Modifies: A,X
	.macro	FSVScroll_SwitchScreenSpare
	lda	fsvscroll_current_screenpage	; Get current page.
	ldx	fsvscroll_spare_screenpage	; Get spare page.
	stx	fsvscroll_current_screenpage	; And now switch.
	sta	fsvscroll_spare_screenpage
	.endmacro

;;; Default function to draw the next line when scrolling up. If the end is reached the scrolling is restartet.
	.global	_fsvscroll_default_draw_next_up_line_and_rotate

;;; Copy the next line to the bottom of the current screen. Both characters and colour is copied.
;;; This function uses the fsvscroll_current_screenpage pointer.
;;; Input: fsvscroll_current_screenpage
;;; Output: Z flag=0: end of scroll frame reached, Z flag=1: end not yet reached
;;; Modifies: A,Y
	.global	fsvscroll_draw_next_line_at_bottom


;;; Reset pointers pointing to the next line to be copied.
;;; This will reset fsvscroll_character_ptr and fsvscroll_colour_ptr. The values are taken from the address variables. It is used by the FSVScroll_Initialise macro.
;;; Input: fsvscroll_character_addr, fsvscroll_colour_addr
;;; Output: fsvscroll_character_ptr, fsvscroll_colour_ptr
;;; Modifies: A
	.global	fsvscroll_reset_scroll_ptrs

;;; Scroll up with the option of giving source and destination pages.
;;; If you like to copy the colour RAM one line up than use A=X=$D8.
;;; Input: A=source page, X=destination page
;;; Output: -
;;; Modifies: A/X/Y
	.import	fsvscroll_copy_line_up
