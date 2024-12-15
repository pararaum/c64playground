;-----------------------------------------------------------------------
; draw_window_frame
; Draws a frame around the window parameters
;
; Jan-2022 V0.3
; Wilfried Elmenreich
; License: The Unlicense
;
; Needs to be linked with window_parameters.o
;-----------------------------------------------------------------------

.include "LAMAlib.inc"

.export _draw_frame_sr

.export _frame_upper_left
.export _frame_upper_right
.export _frame_lower_left
.export _frame_lower_right
.export _frame_vertical
.export _frame_horizontal
.export _frame_fillchar
.export _frame_color

.import _window_x1,_window_y1,_window_x2,_window_y2

.import _mul40_tbl_lo,_mul40_tbl_hi
.importzp _llzp_word1,_llzp_word2

scrptr:=_llzp_word1
colptr:=_llzp_word2
scrpage =   $288

_frame_upper_left:  .byte $70
_frame_upper_right: .byte $6e
_frame_lower_left:  .byte $6d
_frame_lower_right: .byte $7d
_frame_vertical:    .byte $5d
_frame_horizontal:  .byte $40
_frame_fillchar:    .byte $20
_frame_color:       .byte $01

_draw_frame_sr:
	ldy _window_y1
	ldx _window_x1
	dex
	txa
	clc
	adc _mul40_tbl_lo-1,y
	sta scrptr
	sta colptr
	php
	lda _mul40_tbl_hi-1,y
	tax
	adc scrpage
	sta scrptr+1
	plp	;recover carry bit
	txa
	adc #$d8
	sta colptr+1

	lda _window_x2
	;clc	;carry is clear
	sbc _window_x1	;subtract _window_x1-1 because of carry	
	adc #2	
	tay
	sty line_width

	lda _frame_upper_right
	sta (scrptr),y
	lda _frame_color
	sta (colptr),y

	jmp dec_n_check
toploop:
	lda _frame_horizontal
	sta (scrptr),y
	lda _frame_color
	sta (colptr),y

dec_n_check:
	dey
	bne toploop

	sta (colptr),y	;accu still contains the color
	lda _frame_upper_left
	sta (scrptr),y

	ldx _window_y1
	dex
	dex
	jmp nextline

vertical_loop:
	ldy #00
	lda _frame_vertical
	sta (scrptr),y
	lda _frame_color
	sta (colptr),y
line_width=*+1
	ldy #10
	sta (colptr),y
	lda _frame_vertical
	sta (scrptr),y
nextline:
	lda scrptr
	clc
	adc #40
	sta scrptr
	sta colptr
	if cs
	  inc scrptr+1
	  inc colptr+1
	endif
	inx
	cpx _window_y2
	bcc vertical_loop

skipvertical:
	ldy line_width
	lda _frame_lower_right
	sta (scrptr),y
	lda _frame_color
	sta (colptr),y

	jmp dec_n_check2
bottomloop:
	lda _frame_horizontal
	sta (scrptr),y
	lda _frame_color
	sta (colptr),y

dec_n_check2:
	dey
	bne bottomloop

	sta (colptr),y	;accu still contains the color
	lda _frame_lower_left
	sta (scrptr),y
	
	rts