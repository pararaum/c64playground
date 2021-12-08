; Swinging rasterbars with front-back effect
;
; The screen content during the rasterbars being in front is blanked out by switching the charset to an area with all 0
; Other than switching the screen pointer, switching the charset does take effect immediately
;
; Code: Wil
; Version 0.1

.include "LAMAlib.inc"

; =====================================================================================
; initialization part
; =====================================================================================

        memset $3800,$3fff,0    ;ensure the alternative charset is empty

	textcolor 1
	clrscr
	newline
	newline	
	newline	
	primm "               vcc rules!"

; =====================================================================================
; stable raster part
; =====================================================================================

rasterline = 42	; rasterline defines where our sync process starts
; rasterline must not be values with a badline or a badline in the following 2 lines
; badlines are lines where the numer $0x07 equals the content of $D011 & 0x07
; standard setting of $D011=$1B, %00011011
; this lines between 50 and 249 with line mod 8 = 3, 4 or 5 won't work

        sei
	pokew $314,irq
        set_raster_irq rasterline
	sta $d019
        cli
	rts

irq:
	;we are somewhere in the middle of the rasterline 
	;38 cycles for the ROM IRQ routine + 0..7 jitter
	;next irq in current line+2 should hit it exactly

	lda #<irq2      ; 2 cycles
	sta $314	; 4
	lda #>irq2	; 2
	sta $315	; 4
	tsx		; 2
	store X		; 4
	lda #1		; 2 preload for acknowledging raster irq
	delay_cycles      6
			;--
			;26 cycles, we are now surely already in rasterline+1

	inc $d012	; 6 set next irq for rasterline+2
	sta $d019       ; 4 acknowledge IRQ
	cli		; 2

	;until now between 76 and 83 cycles passed since the first rasterline
	;the next one comes after 126 cycles or 83 cycles (if there was a badline)

	.repeat 26	;irq will happen for sure within these nops
	nop
	.endrep
        
irq2:
        ;38 or 39 cycles passed in this line
	restore X	; 2
	txs		; 2
	delay_cycles	  8
	lda #1		; 2 preload A
	bit $01         ; 3 just for 3 cycles
	ldx $d012	; 4
	cpx $d012	; 4
			;--
			;25 we are now already in next line or not 
	beq stable_raster   ;3 cycles if before, 2 otherwise

	;we are now definitely 2 cycles into rasterline
stable_raster:

; -------------------------------------------------------------------------------------
; end stable raster part
; -------------------------------------------------------------------------------------

.macro calc_linecycles rasterline
  .if (rasterline & 7)=3
    lc .set 20
    .out "bad line"
  .else
    lc .set 63
    .out "normal line"
  .endif
.endmacro

.macro busy_wait from,to
  .if from<to
    .if (from & 7)=3
      delay_cycles 20
    .else
      delay_cycles 63
    .endif
    busy_wait from+1,to
  .endif
.endmacro

chars_hidden=%00011110   ;$400 Screen, $3800 Charset
chars_shown =%00010100   ;$400 Sceen, $1000 Charset

col_border=0
col_1     =7
col_2     =6
col_3     =7
col_4     =6
col_5     =7
col_6     =6
col_7     =8
col_8     =10
col_9     =7
col_10    =7

; col_border=0
; col_1     =7
; col_2     =7
; col_3     =10
; col_4     =8
; col_5     =2
; col_6     =2
; col_7     =8
; col_8     =10
; col_9     =7
; col_10    =7

rline .set 55   ;irq1+4
startline .set 56

calc_linecycles rline

;line 0: pre-load A and X, set $d018 at the end
	lda #col_1       ; 2
	ldy front_back   ; 4
	delay_cycles	 lc-6-4-7



busy_wait rline,startline

rline .set startline-1
	sty $d018

rline .set rline+1
calc_linecycles rline

;line 1
	sta $d020	; 4
	sta $d021	; 4
	lda #col_2	; 2
	delay_cycles     lc-10

rline .set rline+1
calc_linecycles rline

;line 2
	sta $d020	; 4
	sta $d021	; 4
	lda #col_3	; 2
	delay_cycles     lc-10

rline .set rline+1
calc_linecycles rline
	
;line 3
	sta $d020	; 4
	sta $d021	; 4
	lda #col_4	; 2
	delay_cycles     lc-10

rline .set rline+1
calc_linecycles rline

;line 4
	sta $d020	; 4
	sta $d021	; 4
	lda #col_5	; 2
	delay_cycles     lc-10

rline .set rline+1
calc_linecycles rline

;last line
	sta $d020	 ; 4
	sta $d021	 ; 4
	lda #col_border  ; 2
	ldy #chars_shown ; 2
	delay_cycles      lc-16
	sty $d018        ; 4
	sta $d020	 ; 4
	sta $d021	 ; 4

	poke 53281,6

midpoint = (y_sineend-y_sine)/2
loopidx=*+1
	ldx #00
        ldy y_sine,x   ;the line where the next rasterline will start
	ldy #51
        inx
	cpx #y_sineend-y_sine
        bcc noreset
        ldx #00	
noreset:
	stx loopidx
	lda #chars_shown
	cpx #midpoint
	bcc firsthalf
	lda #chars_hidden
firsthalf:
	sta front_back

	sty $d012

	pokew $314,irq
	asl $d019
	jmp $ea31

front_back: .byte chars_shown

colors:
        .byte $00
        .byte $07,$07,$0a,$08,$02
        .byte $02,$08,$0a,$07,$07
colorsend:

y_sine:  .byte 87,87,87,87,85,85,83,83,81,81,79,77,75,73,71,69,69,67,65,63,61,59,57,55,55,53,53,51,51,49,49,49,49,49,49,49,51,51,53,53,55,55,57,59,61,63,65,67,67,69,71,73,75,77,79,81,81,83,83,85,85,87,87,87
y_sineend:

