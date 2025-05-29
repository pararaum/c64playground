;***********************************************************************
;* Module: colorfade
;* Version 0.1
;* by Wil
;
; to be configured and included with
; .scope colorfade
; .include "m_colorfade.s"
; .endscope
;
; usage from the main program
;
; m_run colorfade    ;fades the colors

.include "LAMAlib.inc"

;***********************************************************************
;* parameters - can be overwritten from main
;* without a default value the constant must be set by the main program

def_const FADE_DELAY,10
def_const WORKING_STEPS,1
def_const LINES,25

def_const COLF0,0	;what follows after color 0
def_const COLF1,15
def_const COLF2,9
def_const COLF3,14
def_const COLF4,6
def_const COLF5,11
def_const COLF6,11
def_const COLF7,8
def_const COLF8,9
def_const COLF9,11
def_const COLF10,2
def_const COLF11,0
def_const COLF12,11
def_const COLF13,5
def_const COLF14,6
def_const COLF15,12

;***********************************************************************
;* module implementation

init:
exit:
        rts

run:
delay_ctr=*+1
	lda #FADE_DELAY+1
	dec delay_ctr
.if WORKING_STEPS=1
	longif eq
	  poke delay_ctr,FADE_DELAY+1
	  for X,39,downto,0
	    .repeat LINES,l
	      lda $d800+l*40,x
	      and #15
	      tay
	      lda color_transition_table,y
	      sta $d800+l*40,x
	    .endrep
	  next
	endif
.else
	cmp #WORKING_STEPS
	bcs exit
.repeat WORKING_STEPS,w

	cmp #w
	longif eq 

.if w=0
	poke delay_ctr,FADE_DELAY+1 
.endif

	  for X,39,downto,0
	    .if w=WORKING_STEPS-1
		.repeat LINES - w*(LINES+WORKING_STEPS-1)/WORKING_STEPS,l
      	          lda $d800+(l+w*(LINES+WORKING_STEPS-1)/WORKING_STEPS)*40,x
      	          and #15
      	          tay
      	          lda color_transition_table,y
      	          sta $d800+(l+w*(LINES+WORKING_STEPS-1)/WORKING_STEPS)*40,x
	        .endrep
	    .else
	        .repeat (LINES+WORKING_STEPS-1)/WORKING_STEPS,l
      	          lda $d800+(l+w*(LINES+WORKING_STEPS-1)/WORKING_STEPS)*40,x
      	          and #15
      	          tay
      	          lda color_transition_table,y
      	          sta $d800+(l+w*(LINES+WORKING_STEPS-1)/WORKING_STEPS)*40,x
      	        .endrep
	    .endif
	  next	
	  rts
	endif
.endrep
.endif
        rts

color_transition_table:
.byte COLF0, COLF1, COLF2, COLF3, COLF4, COLF5, COLF6, COLF7, COLF8, COLF9, COLF10, COLF11, COLF12, COLF13, COLF14, COLF15
