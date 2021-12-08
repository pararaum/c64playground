; Wipe out and wipe over effect for a text area defined by a begin and end screenline
;
; Code: Wil
; Version 0.1

.include "LAMAlib.inc"

.import _frame0000

.export wipe_out,wipe_over,advance_display_state

;define begin and end line of copy area
begin_line=13
end_line  =23
DISPLAY_DELAY=200

.ifndef SCREEN_BASE
SCREEN_BASE=$0400
.endif

.ifndef COLRAM_BASE
COLRAM_BASE=$D800
.endif

main:
	pokew $d020,0
	textcolor 0
	clrscr
        memcopy _frame0000+2,SCREEN_BASE, 12*40
        memcopy _frame0000+1002,COLRAM_BASE, 12*40

	do
	  jsr advance_display_state
	  wait_for_rasterline 0
	loop

advance_display_state:
state=*+1
	ldx #00
	switch X
	case 0
	  ldax #_frame0000+2+40*(begin_line-1)
          jsr wipe_over
	  if cs
	    inc state
          endif
          break
	case 1
	  dec delay_timer
	  if eq
	    poke delay_timer,DISPLAY_DELAY
	    inc state
          endif
          break
	case 2
	  jsr wipe_out
	  if cs
	    inc state
          endif
          break
	case 3
	  ldax #_frame0000+2004+40*(begin_line-1)
          jsr wipe_over
	  if cs
	    inc state
          endif
          break
	case 4
	  dec delay_timer
	  if eq
	    poke delay_timer,DISPLAY_DELAY
	    inc state
          endif
          break
	case 5
	  jsr wipe_out
	  if cs
	    inc state
          endif
          break
	case 6
	  ldax #_frame0000+4006+40*(begin_line-1)
          jsr wipe_over
	  if cs
	    inc state
          endif
          break
	case 7
	  dec delay_timer
	  if eq
	    poke delay_timer,DISPLAY_DELAY
	    inc state
          endif
          break
	case 8
	  jsr wipe_out
	  if cs
	    inc state
          endif
          break
	case 9
	  ldax #_frame0000+6008+40*(begin_line-1)
          jsr wipe_over
	  if cs
	    inc state
          endif
          break
	case 10
	  dec delay_timer
	  if eq
	    poke delay_timer,DISPLAY_DELAY
	    inc state
          endif
          break
	case 11
	  jsr wipe_out
	  if cs
	    inc state
          endif
          break
	case 12
	  poke state,0
	  ;break	;the last break is not needed and it would generate an unnecessary jmp *+3 here
	endswitch
        rts

delay_timer: .byte DISPLAY_DELAY


; wipe clean an area by overwriting the color RAM
; constants begin_line and end_line define the screen area to be wiped
; each call wipes one diagonal stripe
; when the last part is wiped the fuction returns with a set carry
wipe_out:
        ldax #COLRAM_BASE+40*(begin_line-1)
        stax smaddr

        ldx startcol
        for y,begin_line,to,end_line
          cpx #00
          if pl
            cpx #40
            if cc
              lda #00
              smaddr=*+1
              sta $affe,x 
            endif
          endif
          lda smaddr
          clc
          adc #40
          sta smaddr
          if cs
            inc smaddr+1
          endif
          dex
        next
        inc startcol
        lda startcol
        cmp #40+end_line-begin_line
        if cs
          poke startcol,0
        endif
        rts

; display screen graphics 
; the graphics are updated one diagonal line at a time, creating a wipe-in-effect
; constants begin_line and end_line define the screen area to be written to
; each call updates one diagonal stripe

wipe_over:
	stax scr_src
	clc
	adcax #1000
	stax col_src

        ldax #SCREEN_BASE+40*(begin_line-1)
        stax scraddr

        ldax #COLRAM_BASE+40*(begin_line-1)
        stax coladdr

        ldx startcol
        for y,begin_line,to,end_line
          cpx #00
          if pl
            cpx #40
            if cc
              scr_src=*+1
              lda $affe,x
              scraddr=*+1
              sta $affe,x 
              col_src=*+1
              lda $affe,x
              coladdr=*+1
              sta $affe,x 
            endif
          endif
          lda scraddr
          clc
          adc #40
          sta scraddr
          sta coladdr
          if cs
            inc scraddr+1
            inc coladdr+1
          endif
	  lda scr_src
	  clc
          adc #40
	  sta scr_src
          if cs
            inc scr_src+1
          endif
	  lda col_src
	  clc
          adc #40
	  sta col_src
          if cs
            inc col_src+1
          endif
          dex
        next
        inc startcol
        lda startcol
        cmp #40+end_line-begin_line
        if cs
          poke startcol,0
        endif
        rts

startcol: .byte 0