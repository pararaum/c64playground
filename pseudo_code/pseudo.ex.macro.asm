;;; asm6502 -l /dev/stderr -e -b 0x07ff pseudo.ex.macro.asm

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

lfsrE:	P_transfer $fe, $fc
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
	eor .bitlist,x
	sta ($fe),y
	lda #%00010000		;Multicolour bitmap mode.
	eor $d016
	sta $d016
	P_transfer $fc, $fe
	P_sub $60, $fe
	P_branchNZ $fe, .l2
	lda #$01		;Zero is never reached therefore hardcoded here.
	eor BITMAPPTR
	sta BITMAPPTR
	rts
.bitlist: .byte $01, $02, $04, $08, $10, $20, $40, $80

;;; http://users.ece.cmu.edu/~koopman/lfsr/16.txt
feedbt:	.word $8016, $801C, $801F, $8029, $805E, $806B, $8097, $809E, $80A7, $80AE, $80CB, $80D0, $80D6, $80DF, $80E3, $810A, $810C, $8112, $8117, $812E, $8136, $8142, $8148, $8150, $8172, $818E, $81A5, $81B4, $81B8, $81C3, $81C6, $81CF, $81D1, $81EE, $81FC, $8214, $822B, $8233, $8241, $8244, $8248, $825F, $8260, $8299, $82A3, $82B4, $82C3, $82E1, $82EE, $82F5, $8320, $8325, $8329, $8345, $8361, $83B5, $83B6, $83BC, $83C1, $83F8, $8406, $8430, $845F, $846A, $846F, $8471, $8478, $847D, $849C, $84BE, $84C5, $84D2, $84D7, $84E1, $84E2, $84F3, $84F9, $853E, $8540, $855D, $8562, $8580, $8589, $858A, $85A8, $85AE, $85E6, $85E9, $85F2, $8607, $860E, $8610, $8634, $8638, $863D, $8646, $864A, $8651, $8657, $8679
feedbt_end:


main:	sei
	lda #$35   ;Turn off the BASIC and KERNAL rom.
        sta $01
	lda #%00111011		;Hires and 25 lines.
	sta $d011
	lda #%00010000		;Multicolour bitmap mode.
	ora $d016
	sta $d016
	jsr initialise_bitmap_and_screenptr
	jsr clearscreen
	nop
	lda #$04
	sta $d020
	sta $d021
	nop
	P_loadi $20, feedbt
	P_loadi $22, feedbt_end
	P_loadi $24, 2
.l1:	nop
	lda $d012
	pha
	P_load $20, $fe
	jsr lfsrE
;;; 	inc $d020
	lda $d012
	sta $fe
	pla
	sta $ff
	;; 	jsr lfsrE
	P_add $24, $20
	P_transfer $20, $fe
	P_sub $22, $fe
	P_branchNZ $fe, .l1
	nop
	lda #$37		;Turn on ROMs.
	sta $01
	cli
	rts
