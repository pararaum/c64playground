;;; asm6502 -l /dev/stderr -e -b 0x07ff pseudo.ex.macro.asm
RFEED = $20			;Pointer to feedback terms.
RFEEDEND = $22			;Pointer to end of feedback terms.
CONST02 = $02			;Here we store a constant '2'.
RFEEDBACKTERM = $80		;Register for feedbackterm
RFEEDBACKVAL = $82		;Current feedbackterm value
RBITMAPPTR = $84		;Pointer to bitmap

	.org $0801 - 2
	.word $0801

basic:	.word end_of_basic
	.word main
	.byte $9e,$20,$c2
	.byte "(57)",$aa,"256",$ac,$c2,"(58)"
end_of_basic:
	.byte 0,0,0

	INCLUDE "macro.pseudo.inc"

BITMAPPTR = $a000
SCREENPTR = $8c00
	INCLUDE "../include/bitmap_n_screen.inc"

clearscreen:	lda #$E2
	ldx #0
.l1	sta SCREENPTR,x
	sta SCREENPTR+$100,x
	sta SCREENPTR+$200,x
	sta SCREENPTR+$300,x
	pha
	lda #1
	sta $d800,x
	sta $d900,x
	sta $da00,x
	sta $db00,x
	pla
	dex
	bne .l1
	P_loadi $60, $a000
	P_loadi $62, $c000
	P_loadi $64, $0000
	P_loadi $5e, $0002
.l2:	P_store $64, $60
	P_add  $5e, $60
	P_transfer $60, $fe
	P_sub $62, $fe
	P_branchNZ $fe, .l2
	rts

lfsrO:	P_transfer $fe, $fc
	P_transfer $fe, $60
	P_loadi $62, BITMAPPTR
.l2:
	ldx $fc
	P_shiftr $fc
	txa
	and #$01
	beq .l1
	P_eor $60, $fc
.l1:
	P_transfer $fc, $fe
	P_shiftr $fe
	P_shiftr $fe
	P_shiftr $fe
	P_add $62, $fe
	lda $fc
	and #$07
	tax
	ldy #0
	lda ($fe),y
	ora .bitlist,x
	sta ($fe),y
	P_transfer $fc, $fe
	P_sub $60, $fe
	P_branchNZ $fe, .l2
	lda #$ff		;Zero is never reached therefore hardcoded here.
	sta BITMAPPTR
	rts
.bitlist: .byte $01, $02, $04, $08, $10, $20, $40, $80

lfsrA:	P_transfer $fe, $fc
	P_transfer $fe, $60
	P_loadi $62, BITMAPPTR
.l2:
	ldx $fc
	P_shiftr $fc
	txa
	and #$01
	beq .l1
	P_eor $60, $fc
.l1:
	P_transfer $fc, $fe
	P_shiftr $fe
	P_shiftr $fe
	P_shiftr $fe
	P_add $62, $fe
	lda $fc
	and #$07
	tax
	ldy #0
	lda ($fe),y
	and .bitlist,x
	sta ($fe),y
	P_transfer $fc, $fe
	P_sub $60, $fe
	P_branchNZ $fe, .l2
	lda #$00		;Zero is never reached therefore hardcoded here.
	sta BITMAPPTR
	rts
.bitlist: .byte ~$01, ~$02, ~$04, ~$08, ~$10, ~$20, ~$40, ~$80

lfsrE:	P_transfer $fe, RFEEDBACKTERM
	P_transfer $fe, RFEEDBACKVAL
	P_loadi RBITMAPPTR, BITMAPPTR
.l2:
	ldx RFEEDBACKTERM
	P_shiftr RFEEDBACKTERM
	txa
	and #$01
	beq .l1
	P_eor RFEEDBACKVAL, RFEEDBACKTERM
.l1:
	P_transfer RFEEDBACKTERM, $fe
	P_shiftr $fe
	P_shiftr $fe
	P_shiftr $fe
	P_add RBITMAPPTR, $fe
	lda RFEEDBACKTERM
	and #$07
	tax
	ldy #0
	lda ($fe),y
	eor .bitlist,x
	sta ($fe),y
	P_transfer RFEEDBACKTERM, $fe
	P_sub RFEEDBACKVAL, $fe
	P_branchNZ $fe, .l2
	lda #$01		;Zero is never reached therefore hardcoded here.
	eor BITMAPPTR
	sta BITMAPPTR
	rts
.bitlist: .byte $01, $02, $04, $08, $10, $20, $40, $80

;;; http://users.ece.cmu.edu/~koopman/lfsr/16.txt
feedbt:	.word $8016, $801C, $801F, $8029, $805E, $806B, $8097, $809E, $80A7, $80AE, $80CB, $80D0, $80D6, $80DF, $80E3, $810A, $810C, $8112, $8117, $812E, $8136, $8142, $8148, $8150, $8172, $818E, $81A5, $81B4, $81B8, $81C3, $81C6, $81CF, $81D1, $81EE, $81FC, $8214, $822B, $8233, $8241, $8244, $8248, $825F, $8260, $8299, $82A3, $82B4, $82C3, $82E1, $82EE, $82F5, $8320, $8325, $8329, $8345, $8361, $83B5, $83B6, $83BC, $83C1, $83F8, $8406, $8430, $845F, $846A, $846F, $8471, $8478, $847D, $849C, $84BE, $84C5, $84D2, $84D7, $84E1, $84E2, $84F3, $84F9, $853E, $8540, $855D, $8562, $8580, $8589, $858A, $85A8, $85AE, $85E6, $85E9, $85F2, $8607, $860E, $8610, $8634, $8638, $863D, $8646, $864A, $8651, $8657, $8679
feedbt_end:

spritedata:	
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $01, $54, $00 
	.byte $06, $A9, $00 
	.byte $1A, $AA, $40 
	.byte $1A, $5A, $90 
	.byte $69, $06, $90 
	.byte $69, $06, $90 
	.byte $69, $06, $90 
	.byte $69, $06, $90 
	.byte $69, $06, $90 
	.byte $1A, $5A, $90 
	.byte $1A, $AA, $40 
	.byte $06, $A9, $00 
	.byte $01, $54, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0000.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $54, $00 
	.byte $01, $A9, $00 
	.byte $06, $A9, $00 
	.byte $01, $69, $00 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $05, $69, $40 
	.byte $1A, $AA, $90 
	.byte $1A, $AA, $90 
	.byte $05, $55, $40 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0016.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $01, $55, $00 
	.byte $06, $AA, $40 
	.byte $1A, $AA, $90 
	.byte $1A, $56, $90 
	.byte $05, $06, $90 
	.byte $00, $06, $90 
	.byte $00, $1A, $40 
	.byte $00, $6A, $40 
	.byte $01, $A5, $40 
	.byte $06, $95, $90 
	.byte $1A, $AA, $90 
	.byte $1A, $AA, $90 
	.byte $05, $55, $40 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $002c.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $55, $00 
	.byte $01, $AA, $40 
	.byte $06, $AA, $90 
	.byte $01, $56, $90 
	.byte $00, $16, $90 
	.byte $00, $6A, $40 
	.byte $00, $6A, $40 
	.byte $00, $16, $90 
	.byte $04, $06, $90 
	.byte $19, $56, $90 
	.byte $1A, $AA, $90 
	.byte $06, $AA, $40 
	.byte $01, $55, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0042.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $15, $00 
	.byte $00, $6A, $40 
	.byte $00, $6A, $40 
	.byte $01, $AA, $40 
	.byte $06, $9A, $40 
	.byte $06, $5A, $40 
	.byte $1A, $5A, $50 
	.byte $6A, $AA, $A4 
	.byte $6A, $AA, $90 
	.byte $15, $5A, $40 
	.byte $01, $AA, $90 
	.byte $01, $AA, $A4 
	.byte $00, $55, $50 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0058.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $05, $55, $40 
	.byte $1A, $AA, $90 
	.byte $1A, $AA, $90 
	.byte $1A, $55, $40 
	.byte $1A, $A9, $00 
	.byte $1A, $AA, $40 
	.byte $1A, $5A, $90 
	.byte $05, $06, $90 
	.byte $00, $06, $90 
	.byte $05, $5A, $90 
	.byte $1A, $AA, $40 
	.byte $1A, $A9, $00 
	.byte $05, $54, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $006e.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $05, $50 
	.byte $00, $5A, $A4 
	.byte $01, $AA, $50 
	.byte $06, $95, $00 
	.byte $06, $95, $00 
	.byte $1A, $6A, $40 
	.byte $1A, $AA, $90 
	.byte $1A, $55, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $55, $A4 
	.byte $06, $AA, $90 
	.byte $01, $AA, $40 
	.byte $00, $55, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0084.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $05, $55, $40 
	.byte $1A, $AA, $90 
	.byte $1A, $AA, $90 
	.byte $19, $56, $90 
	.byte $04, $1A, $40 
	.byte $00, $1A, $40 
	.byte $00, $1A, $40 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $00, $69, $00 
	.byte $01, $A9, $00 
	.byte $01, $A4, $00 
	.byte $00, $50, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $009a.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $55, $00 
	.byte $05, $AA, $50 
	.byte $1A, $AA, $A4 
	.byte $1A, $55, $A4 
	.byte $1A, $55, $A4 
	.byte $06, $AA, $90 
	.byte $06, $AA, $90 
	.byte $1A, $95, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $55, $A4 
	.byte $06, $AA, $A4 
	.byte $01, $AA, $50 
	.byte $00, $55, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $00b0.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $55, $00 
	.byte $01, $AA, $40 
	.byte $06, $AA, $90 
	.byte $1A, $55, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $55, $A4 
	.byte $06, $AA, $A4 
	.byte $01, $A9, $A4 
	.byte $00, $56, $90 
	.byte $00, $56, $90 
	.byte $05, $AA, $40 
	.byte $1A, $A5, $00 
	.byte $05, $50, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $00c6.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $01, $55, $00 
	.byte $06, $AA, $40 
	.byte $06, $AA, $40 
	.byte $01, $A6, $90 
	.byte $01, $A6, $90 
	.byte $06, $96, $A4 
	.byte $06, $AA, $A4 
	.byte $06, $AA, $A4 
	.byte $1A, $55, $A4 
	.byte $6A, $96, $A9 
	.byte $6A, $96, $A9 
	.byte $15, $41, $54 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $00dc.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $15, $55, $00 
	.byte $6A, $AA, $40 
	.byte $6A, $AA, $90 
	.byte $1A, $56, $90 
	.byte $1A, $56, $90 
	.byte $1A, $AA, $90 
	.byte $1A, $AA, $90 
	.byte $1A, $55, $A4 
	.byte $1A, $55, $A4 
	.byte $6A, $AA, $A4 
	.byte $6A, $AA, $90 
	.byte $15, $55, $40 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $00f2.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $01, $55, $50 
	.byte $06, $AA, $A4 
	.byte $1A, $AA, $A4 
	.byte $6A, $56, $A4 
	.byte $69, $01, $A4 
	.byte $69, $00, $50 
	.byte $69, $00, $00 
	.byte $69, $00, $50 
	.byte $6A, $55, $A4 
	.byte $1A, $AA, $A4 
	.byte $06, $AA, $50 
	.byte $01, $55, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0108.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $15, $55, $00 
	.byte $6A, $AA, $40 
	.byte $6A, $AA, $90 
	.byte $1A, $56, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $41, $A4 
	.byte $1A, $56, $90 
	.byte $6A, $AA, $90 
	.byte $6A, $AA, $40 
	.byte $15, $55, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $011e.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $15, $55, $50 
	.byte $6A, $AA, $A4 
	.byte $6A, $AA, $A4 
	.byte $1A, $55, $A4 
	.byte $1A, $69, $A4 
	.byte $1A, $A9, $50 
	.byte $1A, $A9, $50 
	.byte $1A, $69, $A4 
	.byte $1A, $55, $A4 
	.byte $6A, $AA, $A4 
	.byte $6A, $AA, $A4 
	.byte $15, $55, $50 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $0134.
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $15, $55, $50 
	.byte $6A, $AA, $A4 
	.byte $6A, $AA, $A4 
	.byte $1A, $55, $A4 
	.byte $1A, $69, $A4 
	.byte $1A, $A9, $50 
	.byte $1A, $A9, $00 
	.byte $1A, $69, $00 
	.byte $1A, $54, $00 
	.byte $6A, $90, $00 
	.byte $6A, $90, $00 
	.byte $15, $40, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00, $00, $00 
	.byte $00		 ;; In original image at position $014a.

spriteinit:
	lda #$ff
	sta $d015		;Enable all sprites
	sta $d01d		;double width
	sta $d017		;double height
	sta $d01c		;multicolour mode
	lda #$60
	ldy #$00
.l1	lda .posx,y
	sta $d000,y
	lda #$60
	sta $d001,y
	iny
	iny
	cpy #16
	bne .l1
	lda #$00
	sta $d010		;No upper bit for X-position.
	ldx #0
.l2	txa
	sta SCREENPTR+1024-8,x
	inx
	cpx #8
	bne .l2
	lda #3			;Sprite colour is turquoise.
	ldy #7
.l3:	sta $d027,y
	dey
	bpl .l3
	lda #5			;green
	sta $d025
	lda #13			;light green
	sta $d026
	rts
.posx:	.byte 30, 60, 90, 120, 150, 180, 210, 240

display:			;rFE value
	P_push $fe		;Push value
	P_push $fe		;Push value
	P_push $fe		;Push value
	;; X---
	lda $ff
	lsr
	lsr
	lsr
	lsr
	sta $fe
	P_loadi $f8, $8000
	jsr .copy
	P_pull $fe
	;; -X--
	lda $ff
	sta $fe
	P_loadi $f8, $8000+64
	jsr .copy
	P_pull $fe
	;; --X-
	lda $fe
	lsr
	lsr
	lsr
	lsr
	sta $fe
	P_loadi $f8, $8000+2*64
	jsr .copy
	P_pull $fe
	;; ---X
	P_loadi $f8, $8000+3*64
	jsr .copy
	rts
.copy:
	P_loadi $fc, $000f
	P_and $fe, $fc
	P_shiftl $fc		;times 64
	P_shiftl $fc
	P_shiftl $fc
	P_shiftl $fc
	P_shiftl $fc
	P_shiftl $fc
	P_loadi $fa, spritedata
	P_add $fa, $fc
	ldy #63
.l1	lda ($fc),y
	sta ($f8),y
	dey
	bpl .l1
	rts

initgfx:lda #$04
	sta $d020
	sta $d021
	lda #%00111011		;Hires and 25 lines.
	sta $d011
	lda #%00010000		;Multicolour bitmap mode.
	ora $d016
	sta $d016
	jsr initialise_bitmap_and_screenptr
	jsr clearscreen
	jsr spriteinit
	rts
	
main:	sei
	lda #$35   ;Turn off the BASIC and KERNAL rom.
        sta $01
	ldx #$ff		;Reset stack.
	txs
	txa
.l2:	sta $00,x
	sta $0100,x
	dex
	cpx #1
	bne .l2
	jsr initgfx
	P_loadi RFEED, feedbt
	P_loadi RFEEDEND, feedbt_end
	P_loadi CONST02, 2
.l1:	P_load RFEED, $fe
	nop
	jsr display
	P_load RFEED, $fe
	nop
	jsr lfsrE
	P_add CONST02, RFEED
	P_transfer RFEEDEND, $fe
	P_sub RFEED, $fe
	P_branchNZ $fe, .l1
	nop
	lda #$37		;Turn on ROMs.
	sta $01
	lda #0
	pha
	brk
	jmp ($fffe)
	cli
	rts
