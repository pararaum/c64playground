; Swinging rasterbars with front-back effect
;
; The screen content during the rasterbars being in front is blanked out by switching the charset to an area with all 0 byte
; Other than switching the screen pointer, switching the charset does take effect immediately
;
; Code: Wil
; Version 0.1

.include "LAMAlib.inc"

.export install_rasterbars,music_routine

;*********************************
;** TODO: lines < 44 need to be calculated separately
;**       macro  calc_linecycles should be aware of border
;**       macro  busy_wait       should be aware of border
;**       recalculate sine values
;*********************************

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
; rasterline+2 must not be a badline
; badlines are lines between 50 and 249 where the numer $0x07 equals the content of $D011 & 0x07
; standard setting of $D011=$1B, %00011011
; thus lines between 50 and 249 with line mod 8 = 1 won't work

install_rasterbars:
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

.assert <* < 253, error, "Branch for synchronisation goes over memory page. Add 1-3 padding bytes before this line to fix it!"

	beq stable_raster   ;3 cycles if before, 2 otherwise

	;we are now definitely 2 cycles into rasterline
stable_raster:

; -------------------------------------------------------------------------------------
; end stable raster synchronization
; -------------------------------------------------------------------------------------
raster_bar_addr=*+1
	jmp rasterbars20	;we use jmp to save time, otherwise we cannot handle a badline

; =====================================================================================
; rasterbar movement
; =====================================================================================
rasterbar_movement:
	poke 53281,6

midpoint = (y_sineend-y_sine)/2
loopidx=*+1
	ldx #00
        ldy y_sine,x   ;the line where the next rasterline will start
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

	tya 
	and #7

	cpy #47
	if ge
          cmp #1
          if eq
            dey
	    sty $d012
	    iny
            jmp cont1
          endif
	endif          

	sty $d012
cont1:
	cpy #48
	if ge
	  clc
          adc #48
  	  tay
        endif
	lda rasterbars_ptr_lo-20,y
	sta raster_bar_addr
	lda rasterbars_ptr_hi-20,y
	sta raster_bar_addr+1

	pokew $314,irq
	asl $d019
music_routine=*+1
	jsr $ff40
	jmp $ea31

; =====================================================================================
; rasterbar definitions
; =====================================================================================

col_border=0
col_1     =15
col_2     =7
col_3     =7 
col_4     =10
col_5     =10
col_6     =8
col_7     =2
col_8     =2
col_9     =2
col_10    =8 
col_11    =10
col_12    =10
col_13    =7
col_14    =7
col_15    =15


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

.macro calc_linecycles rasterline
  .if rasterline<50
    lc .set 63
  .elseif (rasterline & 7)=3
    lc .set 20
  .else
    lc .set 63
  .endif
.endmacro

.macro busy_wait from,to
  .if from<to
    .if from<50
      delay_cycles 63
    .elseif (from & 7)=3
      delay_cycles 20
    .else
      delay_cycles 63
    .endif
    busy_wait from+1,to
  .endif
.endmacro

chars_hidden=%00011110   ;$400 Screen, $3800 Charset
;chars_hidden=%00010010   ;$400 Screen, $800 Charset
chars_shown =%00010100   ;$400 Sceen, $1000 Charset

; generator program for pointer list
; 5 m$="<":gosub10:print
; 6 m$=">":gosub10
; 7 end
; 10 k$=""
; 15 fori=20to49:printk$m$"rasterbars"mid$(str$(i),2,3);:k$=",":next
; 20 return

rasterbars_ptr_lo:
.byte <rasterbars20,<rasterbars21,<rasterbars22,<rasterbars23,<rasterbars24,<rasterbars25,<rasterbars26,<rasterbars27,<rasterbars28,<rasterbars29,<rasterbars30,<rasterbars31,<rasterbars32,<rasterbars33,<rasterbars34,<rasterbars35,<rasterbars36,<rasterbars37,<rasterbars38,<rasterbars39,<rasterbars40,<rasterbars41,<rasterbars42,<rasterbars43,<rasterbars44,<rasterbars45,<rasterbars46,<rasterbars47,<rasterbars48,<rasterbars49,<rasterbars50,<rasterbars51,<rasterbars52,<rasterbars53,<rasterbars54,<rasterbars55

rasterbars_ptr_hi:
.byte >rasterbars20,>rasterbars21,>rasterbars22,>rasterbars23,>rasterbars24,>rasterbars25,>rasterbars26,>rasterbars27,>rasterbars28,>rasterbars29,>rasterbars30,>rasterbars31,>rasterbars32,>rasterbars33,>rasterbars34,>rasterbars35,>rasterbars36,>rasterbars37,>rasterbars38,>rasterbars39,>rasterbars40,>rasterbars41,>rasterbars42,>rasterbars43,>rasterbars44,>rasterbars45,>rasterbars46,>rasterbars47,>rasterbars48,>rasterbars49,>rasterbars50,>rasterbars51,>rasterbars52,>rasterbars53,>rasterbars54,>rasterbars55

rasterbars20:
current_rasterline .set 23
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars21:
current_rasterline .set 24
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars22:
current_rasterline .set 25
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars23:
current_rasterline .set 26
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars24:
current_rasterline .set 27
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars25:
current_rasterline .set 28
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars26:
current_rasterline .set 29
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars27:
current_rasterline .set 30
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars28:
current_rasterline .set 31
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars29:
current_rasterline .set 32
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars30:
current_rasterline .set 33
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars31:
current_rasterline .set 34
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars32:
current_rasterline .set 35
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars33:
current_rasterline .set 36
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars34:
current_rasterline .set 37
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars35:
current_rasterline .set 38
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars36:
current_rasterline .set 39
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars37:
current_rasterline .set 40
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars38:
current_rasterline .set 41
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars39:
current_rasterline .set 42
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars40:
current_rasterline .set 43
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars41:
current_rasterline .set 44
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars42:
current_rasterline .set 45
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars43:
current_rasterline .set 46
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars44:
current_rasterline .set 47
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars45:
current_rasterline .set 48
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars46:
current_rasterline .set 49
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars47:
current_rasterline .set 50
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars48:
current_rasterline .set 51
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars49:
current_rasterline .set 51
delay_lines .set 1   ;we have to call the irq one line earlier, because the stable raster does not work with a badline
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars50:
current_rasterline .set 53
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars51:
current_rasterline .set 54
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars52:
current_rasterline .set 55
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars53:
current_rasterline .set 56
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars54:
current_rasterline .set 57
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars55:
current_rasterline .set 58
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

rasterbars56:
current_rasterline .set 59
delay_lines .set 0
.include "rasterbars.inc"
jmp rasterbar_movement

front_back: .byte chars_shown

colors:
        .byte $00
        .byte $07,$07,$0a,$08,$02
        .byte $02,$08,$0a,$07,$07
colorsend:

y_sine:  .byte 109,109,109,108,107,106,105,104,103,101,99,97,95,93,91,88,86,83,80,78,75,72,69,66,63,60,58,55,52,50,47,45,43,41,39,37,35,34,33,32,31,30,29,29,29,29,29,30,31,32,33,34,35,37,39,41,43,45,47,50,52,55,58,60,63,66,69,72,75,77,80,83,86,88,91,93,95,97,99,101,103,104,105,106,107,108,109,109
y_sineend:

