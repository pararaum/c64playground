;;; Use the basic functions to create a brainfuck programm via the listing. Then use SAVE and LOAD normally.
	.include "LAMAlib.inc"
	.include "colors.i"

.MACPACK generic

.export install_irq
.export display_mode

KEYBUFLEN=$c6
KEYBUF=$277

KEY_F1=133
KEY_F2=137
KEY_F3=134
KEY_F4=138
KEY_F5=135
KEY_F6=139
KEY_F7=136
KEY_F8=140

.proc install_irq
	sei
	pokew $314,isr
	cli
	rts
.endproc

.proc isr
	jsr $ea87	;scan keyboard
	ldx KEYBUFLEN
	if ne
	  lda KEYBUF-1,x
	  switch A
	  case KEY_F1
	    poke display_mode,$80
	    lda page
	    add #16
	    cmp #32
	    if cs
	      lda #02
	    endif
change_page:
	    sta page
	    sta $d018	;set charset to VIC Bank+
	    cmp #15
	    if cc
	      lda #$00
	    else
	      lda #$0b
	    endif
	    sta $d020
	    poke $d021,0
	    set_VIC_bank $8000
	    dec KEYBUFLEN
	    break
	  case KEY_F3
	    poke display_mode,$00
	    poke page,16	;dfault value so that next F1 shows first page
	    set_VIC_bank $0000
	    set_VIC_addr $400,$1800
            dec KEYBUFLEN
            break
	  case KEY_F5
	    poke shadow_d021,0
            dec KEYBUFLEN
	    break
	  case KEY_F7
	    inc shadow_d021
            dec KEYBUFLEN
	  endswitch
	endif

	bit display_mode
	if pl
	  lda shadow_d020
	  sta $d020
	  lda shadow_d021
	  sta $d021
	endif

	;copy of original Kernal routine
        jsr $ffea   ; do clock
        lda $cc     ; flash cursor
        bne L_ea61
        dec $cd
        bne L_ea61
        lda #$14
        sta $cd
        ldy $d3
        lsr $cf
        ldx $0287
        lda ($d1),y
        bcs L_ea5c
        inc $cf
        sta $ce
        jsr $ea24
        lda ($f3),y
        sta $0287
        ldx $0286
        lda $ce
L_ea5c: eor #$80
        jsr $ea1c   ; display cursor
L_ea61: lda $01     ; checl cassette sense
        and #$10
        beq L_ea71
        ldy #$00
        sty $c0
        lda $01
        ora #$20
        bne L_ea79
L_ea71: lda $c0
        bne L_ea7b
        lda $01
        and #$1f
L_ea79: sta $01 
L_ea7b:
	;jsr $EA87   ; scan keyboard - we skip this because we have done it earlier
	jmp $ea7e    ; continue with Kernal IRQ routine

page:	.byte 18

.endproc

display_mode: .byte $00