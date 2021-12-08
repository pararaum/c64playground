.include "LAMAlib.inc"
.macpack cbm

.export text_fly_in:=main

STEPS_PHASE0=32
STEPS_PHASE1=18
STEPS_PHASE2=32
STEPS_PHASE3=128

scroll_line=17

DEBUG=1
INVERSE=1
BUSYWAIT=1

.ifndef screen_base
screen_base=$0400
.endif

scroll_default=%11001000

	pokew $d020,0
        poke scrollval,0
        poke speed,0
	poke scrollstate,0
	poke scrollsteps,32

	set_cursor_pos 15,13
	printstr greetings_str

main:
	sei
        ldy scrollval
        wait_for_rasterline 50+scroll_line*8-2
.if DEBUG>0
inc $d020
.endif
        sty $d016
	cli

.if BUSYWAIT=0
	lda #50+scroll_line*8+9
:	cmp $d012
	bcs :-

        ;wait_for_rasterline 50+scroll_line*8+9


.if DEBUG>0
inc $d020
.endif
        poke $d016,scroll_default
.endif

        lda scrollval   
        sec
        sbc speed
        if cc
          adc #8
          sta scrollval
          ;scroll one char
          for X,0,to,38
            lda screen_base+scroll_line*40+1,X
            sta screen_base+scroll_line*40,X
          next
          jsr next_greeting_byte
.if INVERSE>0
	  eor #$80
.endif
          sta screen_base+scroll_line*40+39
        else
          sta scrollval
        endif

        ldy scrollstate
        switch Y
        case 0         ;we accelerate
          lda scrollsteps
          and #%00000011
          if eq
            lda speed
            cmp #8
            if cc
              adc #1
              sta speed
            endif
	  endif
	  dec scrollsteps
	  if eq
	    poke scrollsteps,STEPS_PHASE1
	    inc scrollstate
	  endif
	case 1
	  dec scrollsteps
	  if eq
	    poke scrollsteps,STEPS_PHASE2
	    inc scrollstate
	  endif
	case 2
          lda scrollsteps
          and #%00000011
          if eq
            lda speed
            if ne
	      sec
              sbc #1
              sta speed
            endif
	  endif
	  dec scrollsteps
	  if eq
	    poke scrollsteps,STEPS_PHASE3
	    inc scrollstate
	  endif
	case 3
	  dec scrollsteps
	  if eq
	    poke scrollsteps,STEPS_PHASE0
	    poke scrollstate,0
	  endif
	endswitch
.if DEBUG>0
dec $d020
dec $d020
.endif

.if BUSYWAIT>0
	ldx #$50
:	dex
	bne :-	
        poke $d016,scroll_default
.if DEBUG>0
inc $d020
.endif
.endif

        jmp main
        
scrollval:   .byte 7
scrollstate: .byte 0
scrollsteps: .byte 32
speed:       .byte 0

.proc next_greeting_byte
addr=*+1
redo:
	lda greetings_txt
	
	if eq
	  cmp mode
	  if eq
	    pokew addr,greetings_txt
	    poke mode,1
.if ::DEBUG>0
inc $d021
.endif
	    jmp redo
	  endif
          poke mode,0
	  inc16 addr
	  jmp redo
	endif

	ldx mode
	switch X
	case 0  ;init countdown spaces mode 
	  sta spacecount
          poke mode,2
	  jmp return_space
	case 1	;literal mode
          inc16 addr
	  rts	;return A
	case 2  ;countdown spaces mode 
return_space:
	  dec spacecount
	  if eq
	    poke mode,1
            inc16 addr
	  endif
	  lda #$20
	  rts
	endswitch
	rts

mode:       .byte 01
spacecount: .byte 00
greetings_txt:
.include "greetings.inc"

.endproc

greetings_str:
	.byte 18,71,82,69,69,84,73,78,71,83,32,84,79,00