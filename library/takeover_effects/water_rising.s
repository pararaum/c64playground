;==========================================================
; Water Rising takeover effect
; Code by Wil 2022
; Version 0.1
; License: The Unlicense
;==========================================================

        ;.include       "t7d/kernal.i"
        .include        "LAMAlib.inc"

        .import TAKEOVER_SCREENBASE
        .importzp       ptr1
        .importzp       tmp1

        .export takeover_water_rising
        .export init_takeover_water_rising
        .export update_takeover_water_rising
	.export takeover_water_level := waterline

TAKEOVER_DISSOLVE_SPEED=0
WATERLINE_HIGH=$05
WATERLINE_LOW=$F8
RISING_DELAY=$03

UPPER_BG=6  ;background above waterline
UPPER_FR=14 ;frame color above waterline


wave_delay=4

        .code

;----------------------------------------------------------
; init code
;----------------------------------------------------------
.proc   init_takeover_water_rising

        poke takeover_water_rising,1  ;Effekt is active
        poke 53265,91    ;turn on ECM mode
        poke updown_counter,$80
        poke waterline,$ee

        poke $d020,UPPER_FR
        lda #UPPER_BG
        sta $d021
        sta $d022
        sta $d023
        sta $d024

        ;set high-bits in screen area   
        pokew ptr1,TAKEOVER_SCREENBASE

        ldy #0
        sty ecm_mask
        sty end_idx
        ldx #2  ;2,1,0 are normal iterations, then comes the $e8 loop

screen_loop:
        lda (ptr1),y
        and #%00111111
ecm_mask=*+1
        eor #00
        sta (ptr1),y
        
        tya
        and #$01        ;is Y an odd number?
        if ne
          lda ecm_mask
          clc
          adc #$40
          sta ecm_mask
        endif

        iny
end_idx=*+1
        cpy #00
        bne screen_loop

        inc ptr1+1
        dex
        bpl screen_loop
        poke end_idx,$e8   ;last $e8 bytes from screen base + $300...$3e8 
        tya                ;check if y is $00 or $e8
        beq screen_loop    ;go into loop one more time if y was $00
        rts
.endproc

;----------------------------------------------------------
; update code, call once per frame
;----------------------------------------------------------
.proc update_takeover_water_rising

        poke $d020,UPPER_FR
        lda #UPPER_BG
        sta $d021
        sta $d022
        sta $d023
        sta $d024

cycle:  ldx #00
        lda cyctable_h,x
        sta cycljmp+2
        lda cyctable_l,x
        sta cycljmp+1

::waterline=*+1
        ldy #$ee        ;water should go between $ee and $30
wait0:
        cpy $d012         ;warning: this breaks eventually when the IRQ is not synched to screen
        bne wait0
        sei
        iny         ;this is the line where our rasterbar will start.

cycljmp:jsr $c000   ;this address will be overwritten

        inc updown_counter 
        lda updown_counter
        cmp #RISING_DELAY
        bne endofloop
        lda #00
        sta updown_counter
        lda waterline
        cmp #WATERLINE_HIGH
        if cs
          dec waterline
          sec
          sbc #$28
          cmp #200
          if cc
            lsr
            lsr
            lsr
            tay
            lda linelo,y
            sta ptr1
            lda linehi,y
            sta ptr1+1
            ldy #$27
            for y,$27,downto,0
              lda (ptr1),y
              and #%11000000
              ora #$20
              sta (ptr1),y
            next
          endif
          jmp endofloop
        endif
        if cc
          poke takeover_water_rising,0
        endif

endofloop:
        dec wave_counter
	if eq
          lda #wave_delay
          sta wave_counter
          lda cycle+1
          clc
          adc #01
          and #07
          sta cycle+1
	endif
        rts
.endproc

wave_counter: .byte wave_delay
updown_counter: .byte 00

.include "water_rising_generated_cycles.inc"

cyctable_l: .byte <cycle0,<cycle1,<cycle2,<cycle3,<cycle4,<cycle5,<cycle6,<cycle7
cyctable_h: .byte >cycle0,>cycle1,>cycle2,>cycle3,>cycle4,>cycle5,>cycle6,>cycle7

linelo:
.REPEAT 25,I
        .byte <(TAKEOVER_SCREENBASE + I*40)
.ENDREP

linehi:
.REPEAT 25,I
        .byte >(TAKEOVER_SCREENBASE + I*40)
.ENDREP

takeover_water_rising: .byte 1