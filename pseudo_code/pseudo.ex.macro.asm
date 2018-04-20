;;; asm6502 -l /dev/stderr -e -b 0x07ff pseudo.ex.01.asm

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

clearscreen:	lda #$41
	ldx #0
.l1	sta SCREENPTR,x
	sta SCREENPTR+$100,x
	sta SCREENPTR+$200,x
	sta SCREENPTR+$300,x
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

main:	sei
	lda #$35   ;Turn off the BASIC and KERNAL rom.
        sta $01
	lda #%00111011		;Hires and 25 lines.
	sta $d011
	jsr initialise_bitmap_and_screenptr
	jsr clearscreen
	nop
	stx $d020
	sty $d021
	sta $0400
	nop
	P_loadi $fe, $84BE	;see http://users.ece.cmu.edu/~koopman/lfsr/16.txt
	jsr lfsrO
	inc $d020
	P_loadi $fe, $8657
	jsr lfsrA
	inc $d020
	P_loadi $fe, $8029
	jsr lfsrO
	inc $d020
	P_loadi $fe, $855D
	jsr lfsrA
	inc $d020
	nop
	lda #$37		;Turn on ROMs.
	sta $01
	cli
	rts
