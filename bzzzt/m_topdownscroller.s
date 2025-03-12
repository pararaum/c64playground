;***********************************************************************
;* Module: polychars
;* Version 0.34
;* by Wil
;*
;* Purpose:
;* This module provides a routine to hardscroll one or more character
;* strings. Each string will scroll in a predefined column on the screen.
;* Different speeds and string lengths are possible. When all strings
;* have scrolled through, the run procedure returns with a set carry flag.
;*
;* Configuration and Inclusion:
;* To use this module in your program:
;* .scope topdownscroller
;*   vscrolltext1: str_enc0 "\x9ein a galaxy far, far away..."
;*   .include "m_topdownscroller.s"
;* .endscope
;*
;* Main Program Usage:
;* m_init topdownscroller               ; Initializes the screen addresses based on
;* m_run topdownscroller                ; To be run after rasterline 250, returns with carry set if all strings are through
;*
;***********************************************************************

.include "LAMAlib.inc"

;***********************************************************************
;* parameters - can be overwritten from main file
;* without a default value the constant must be set by the main program

def_const SCREEN_BASE,$400

;for each defined vscrolltext1, define DELAY, REPEAT and COLUMN
.repeat 40,i    ;maximum 40 parallel strings
  .ifdef .ident(.sprintf("vscrolltext%d",i+1))
def_const .ident(.sprintf("DELAY%d",i+1)),(4+3*i) .mod 10 + 5   ;number of delay frames between a scroll
def_const .ident(.sprintf("REPEAT%d",i+1)),1                    ;if ne then text is repeated after one scroll
def_const .ident(.sprintf("COLUMN%d",i+1)),(4+3*i) .mod 40      ;if ne then text is repeated after one scroll
  .endif
.endrep

.code

;***********************************************************************
;* Procedure: init
;* Resets the pointers for the strings
;* Resets scroll_done flags
;***********************************************************************
.proc init
        ldx #done_flags_end-done_flags
        lda #0
:       sta done_flags-1,x
        dex
        bne :-
.repeat 40,i    ;maximum 40 parallel strings
  .ifdef .ident(.sprintf("vscrolltext%d",i+1))
                lda #<.ident(.sprintf("vscrolltext%d",i+1))
                sta .ident(.sprintf("tds_ptr%d",i+1))
                lda #>.ident(.sprintf("vscrolltext%d",i+1))
                sta .ident(.sprintf("tds_ptr%d",i+1))+1
  .endif
.endrep
        rts
.endproc

;***********************************************************************
;* Procedure: run
;* Scrolls each string down whenever the respective delay timer
;* expires.
;* Returns with a set carry if all strings have scrolled through,
;* otherwise carry is cleared. A repeaeted string is considered done
;* after the first iteration
;***********************************************************************
run:
.repeat 40,i    ;maximum 40 parallel strings
  .ifdef .ident(.sprintf("vscrolltext%d",i+1))
        do_every .ident(.sprintf("DELAY%d",i+1))

    .if .ident(.sprintf("REPEAT%d",i+1))=0
            lda .ident(.sprintf("done_flag%d",i+1))
            beq :+
            lda #$20
            sta .ident(.sprintf("NEXTCHAR%d",i+1))
            jmp .ident(.sprintf("print_next%d",i+1))
:
    .endif

.ident(.sprintf("load_next%d",i+1)):
.ident(.sprintf("tds_ptr%d",i+1))=*+1
	    lda .ident(.sprintf("vscrolltext%d",i+1))
	    pha
                inc16 .ident(.sprintf("tds_ptr%d",i+1))
	    pla
            if eq
                poke .ident(.sprintf("done_flag%d",i+1)),1
    .if .ident(.sprintf("REPEAT%d",i+1))
                lda #<.ident(.sprintf("vscrolltext%d",i+1))
                sta .ident(.sprintf("tds_ptr%d",i+1))
                lda #>.ident(.sprintf("vscrolltext%d",i+1))
                sta .ident(.sprintf("tds_ptr%d",i+1))+1
	        jmp .ident(.sprintf("load_next%d",i+1))
    .else
                lda #$20
                sta .ident(.sprintf("NEXTCHAR%d",i+1))
    .endif
            endif

	    ;check if character is a color code
            tax
	    lda petscii2color,x
	    if ne
	       sta .ident(.sprintf("NEXTCOL%d",i+1))
	       jmp .ident(.sprintf("load_next%d",i+1))
	    endif
	    txa
	    to_scrcode
	    sta .ident(.sprintf("NEXTCHAR%d",i+1))
               
.ident(.sprintf("print_next%d",i+1)):

    .repeat 24,line
            lda SCREEN_BASE+(23-line)*40+.ident(.sprintf("COLUMN%d",i+1))
            sta SCREEN_BASE+(24-line)*40+.ident(.sprintf("COLUMN%d",i+1))
            lda $d800+(23-line)*40+.ident(.sprintf("COLUMN%d",i+1))
            sta $d800+(24-line)*40+.ident(.sprintf("COLUMN%d",i+1))
    .endrep

.ident(.sprintf("NEXTCHAR%d",i+1))=*+1
            lda #$20
            sta SCREEN_BASE+.ident(.sprintf("COLUMN%d",i+1))
.ident(.sprintf("NEXTCOL%d",i+1))=*+1
            lda #$f
            sta $d800+.ident(.sprintf("COLUMN%d",i+1))
        end_every

.ident(.sprintf("skip%d",i+1)):
  .endif

.endrep

	ldx #done_flags_end-done_flags
	lda #1
:
	and done_flags-1,x
	dex
	bne :-
	lsr	;bit 0 -> C
        rts

done_flags:
.repeat 40,i    ;maximum 40 parallel strings
  .ifdef .ident(.sprintf("vscrolltext%d",i+1))
.ident(.sprintf("done_flag%d",i+1)):
        .res 1
  .endif
.endrep
done_flags_end:

petscii2color:
        .res 5
        .byte 1 ;white
        .res 22
        .byte 2 ;red
	.res 1
        .byte 5 ;green
        .byte 6 ;blue
        .res 32
        .res 32
        .res 32
        .res 1
        .byte 8 ;orange
        .res 14
        .byte 16 ;black
        .res 4
        .byte 9 ;brown
        .byte 10 ;pink
        .byte 11 ;dark gray
        .byte 12 ;mid gray
        .byte 13 ;light green
        .byte 14 ;light blue
        .byte 15 ;light gray
        .byte 4 ;purple
        .res 1
        .byte 7 ;yellow
        .byte 3 ;cyan
        .res 32
        .res 32
        .res 32
