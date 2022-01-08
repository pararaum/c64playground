;-----------------------------------------------------------------------
; windowchrout
; Prints characters that are output via $FFD2 into a textwindow
;
; Jan-2022 V0.1
; Wilfried Elmenreich
; License: The Unlicense
;
; Routine is almost twice as fast as the KERNAL routine
;
; Note that control character keypresses in direct mode are not handled
; via FFD2, therefore pressing for example CLR/HOME will leave the
; window.
;
; The page of the textscreen (stored in $288 / 648) is used to determine
; the output screen, but if you change the screen, the routine
; _enable_chrout2window needs to be called again.
;
; limitations: no reverse mode, no backspace, no insert
;-----------------------------------------------------------------------

.include "LAMAlib.inc"

.macpack longbranch

.export _chrout2window, _enable_chrout2window, _disable_chrout2window

.export _homex     :=homex        ;left column
.export _homey     :=homey        ;upper line
.export _lastcolumn:=lastcolumn   ;right column
.export _lastline  :=lastline     ;lower line

; switch for adding fileio support
support_fileio=1	;set this to 0 if fileio is not needed while windowing is enabled
			;removing fileio support will save 15 cycles and 17 bytes

; the following memory addresses are used the same way the Kernal 
; uses them for output of a character

cursorx  =$D3
cursory  =$D6
charcolor = $286
scrpage =   $288

_enable_chrout2window:
	pokew $0326,_chrout2window

	;move cursorpos into window if necessary
	lda homex
	cmp cursorx
	if cs
	  sta cursorx
	endif

	lda lastcolumn
	cmp cursorx
	if cc
	  sta cursorx
	endif

	lda homey
	cmp cursory
	if cs
	  sta cursory
	endif

	lda lastline
	cmp cursory
	if cc
	  sta cursory
	endif

	;set screen and color pointers to cursor pos
	ldy cursory
	lda mul40_lo,y
	sta scrptr
	sta colptr
	lda mul40_hi,y
	pha
	clc
	adc scrpage
	sta scrptr+1
	pla	;get value of mul40_hi,y again
	;clc
	adc #$D8
	sta colptr+1

	rts

_disable_chrout2window:
	pokew $0326,$F1CA	;Kernal routine for character output

rts_address:
	rts

homex:	    .byte 5
homey:	    .byte 2
lastcolumn: .byte 20
lastline:   .byte 10

;---------------------------------------------------
; poor man's replacement for CHROUT
;---------------------------------------------------
_chrout2window:
.if support_fileio = 1
	pha
	lda $9A		;get device number to write to
	cmp #$03	;screen?
	beq output_to_screen
	bcc datasetteorrsr232	;lower than 3?
	pla
	jmp $EDDD  	;output byte to serial bus
datasetteorrsr232:
	jmp $F1DB	;out byte to device 1 or 2
output_to_screen:
	pla
.endif
	store A		;preserve registers
	store X
	store Y

	bit rts_address	;anded with #%01100000
	bne printablechar  
	cmp #$0d
	bne noret
	jmp chr_return
noret:
	cmp #29		;crsr right
	bne noright
	jmp chr_right
noright:
	cmp #$11	;crsr down
	bne nodown
	jmp chr_down
nodown:
	cmp #$13	;home
	bne nohome
	jmp home_pos
nohome:
	cmp #145	;crsr up
	bne noup
	jmp chr_up
noup:
	cmp #157	;crsr left
	bne noleft
	jmp chr_left
noleft:
	cmp #147	;clrscr
	bne noclrscr
	jmp clr_scr
noclrscr:
	;must be a color code
	ldy #8
checkcolors:
	cmp color_codes,y
	bne nohit
	sty charcolor
	jmp exit_routine
nohit:
	dey
	bpl checkcolors
	sec
	sbc #140
	sta charcolor
	jmp exit_routine

printablechar:
;convert to screencode
	tay             	;update flags
	bmi above128
	and #$3f
above128:
	and #$7f

	ldx cursorx
;copy char
scrptr=*+1
	sta $0400,x

;copy color
	lda charcolor
colptr=*+1
	sta $d800,x

	;fallthrough!

;---------------------------------------------------
;cursor right
;---------------------------------------------------
chr_right:
	lda cursorx
	cmp lastcolumn
	if cs
	  jmp chr_return
	endif
	inc cursorx
        jmp exit_routine

;---------------------------------------------------
;cursor left
;---------------------------------------------------
chr_left:
;go back one char
	lda homex
	cmp cursorx
	bcs up_and_to_end_of_line	;we are already at the left side of the window
	dec cursorx
        jmp exit_routine

up_and_to_end_of_line:
	lda lastcolumn
	sta cursorx

	;fallthrough!

;---------------------------------------------------
;cursor up
;---------------------------------------------------
chr_up:
	lda homey
	cmp cursory
	if cs
	  jmp exit_routine	;we are already at the top of the window
	endif
	dec cursory	

	lda scrptr
	sbc #39		;40-1 because carry is clear
	sta scrptr
	sta colptr
	if cc
	  dec scrptr+1
	  dec colptr+1
	endif
        jmp exit_routine

;---------------------------------------------------
;cursor return
;---------------------------------------------------
chr_return:
	lda homex
	sta cursorx

	;fallthrough!

;---------------------------------------------------
;cursor down
;---------------------------------------------------
chr_down:
	lda cursory
	cmp lastline
	bcc ok_down
	jsr scrollup
	jmp exit_routine
ok_down:
	inc cursory
	lda scrptr
	adc #40		;because carry is clear
	sta scrptr
	sta colptr
	if cs
	  inc scrptr+1
	  inc colptr+1
	endif
	jmp exit_routine

;---------------------------------------------------
;clear screen (or rather the window in our case)
;---------------------------------------------------
clr_scr:
	ldy homey
	lda homex
	clc
	adc mul40_lo,y
	sta scr_clr_ptr2
	sta col_clr_ptr2
	php
	lda mul40_hi,y
	adc scrpage
	sta scr_clr_ptr2+1
	plp	;recover carry bit
	lda mul40_hi,y
	adc #$d8
	sta col_clr_ptr2+1

	lda lastcolumn
	sec
	sbc homex
	sta line_width2

	dey	;because we need to clear one line more

clear_lines:
line_width2=*+1
	ldx #00

clear_line:
	lda #$20
scr_clr_ptr2=*+1
	sta $400,x
	lda charcolor
col_clr_ptr2=*+1	
	sta $d800,x

	dex
	bpl clear_line

	clc
	lda scr_clr_ptr2
	adc #40
	if cs
	  inc scr_clr_ptr2+1
	  inc col_clr_ptr2+1
	endif
	sta scr_clr_ptr2
	sta col_clr_ptr2

	iny
	cpy lastline
	bcc clear_lines

	;fallthrough!

;---------------------------------------------------
;crsr home
;---------------------------------------------------
home_pos:
	lda homex
	sta cursorx
	ldy homey
	sty cursory

	lda mul40_lo,y
	sta scrptr
	sta colptr
	lda mul40_hi,y
	clc
	adc scrpage
	sta scrptr+1
	lda mul40_hi,y
	adc #$d8
	sta colptr+1
	
	;fallthrough!

;---------------------------------------------------
;exit routine
;---------------------------------------------------
exit_routine:
	restore A
	restore X
	restore Y
	clc
	rts

;---------------------------------------------------
;scroll up and clear last line in window
;---------------------------------------------------
scrollup:
	ldy homey
	lda homex
	clc
	adc mul40_lo,y
	sta scr_to_ptr
	sta col_to_ptr
	php
	lda mul40_hi,y
	adc scrpage
	sta scr_to_ptr+1
	sta scr_from_ptr+1
	plp	;recover carry bit
	lda mul40_hi,y
	adc #$d8
	sta col_to_ptr+1
	sta col_from_ptr+1

	lda scr_to_ptr
	adc #40
	sta scr_from_ptr
	sta col_from_ptr
	if cs
	  inc scr_to_ptr+1
	  inc col_to_ptr+1
	endif

	lda lastcolumn
	sec
	sbc homex
	sta line_width

scroll_lines:
	cpy lastline
	bcs clear_last_line

line_width=*+1
	ldx #00

copy_line:
scr_from_ptr=*+1
	lda $428,x
scr_to_ptr=*+1
	sta $400,x
col_from_ptr=*+1
	lda $D828,x
col_to_ptr=*+1
	sta $D800,x	
	dex
	bpl copy_line
	
	lda scr_from_ptr
	sta scr_to_ptr
	sta col_to_ptr
	ldx scr_from_ptr+1
	stx scr_to_ptr+1
	ldx col_from_ptr+1
	stx col_to_ptr+1
	clc
	adc #40
	if cs
	  inc scr_from_ptr+1
	  inc col_from_ptr+1
	endif
	sta scr_from_ptr
	sta col_from_ptr

	iny
	bne scroll_lines

clear_last_line:
	lda scr_to_ptr
	sta scr_clr_ptr
	sta col_clr_ptr
	lda scr_to_ptr+1
	sta scr_clr_ptr+1
	lda col_to_ptr+1
	sta col_clr_ptr+1

	ldx line_width
clr_loop:
	lda #$20
scr_clr_ptr=*+1
	sta $400,x
	lda charcolor
col_clr_ptr=*+1
	sta $d800,x
	dex
	bpl clr_loop

	rts

;---------------------------------------------------
;tables
;---------------------------------------------------

color_codes:
.byte 144,5,28,159,156,30,31,158,129

mul40_lo:
	.repeat 25,I
	.byte <(I*40)
	.endrep

mul40_hi:
	.repeat 25,I
	.byte >(I*40)
	.endrep
	