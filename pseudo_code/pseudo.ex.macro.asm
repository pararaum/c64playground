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
	rts

main:	nop
	lda #%00111011		;Hires and 25 lines.
	sta $d011
	jsr initialise_bitmap_and_screenptr
	jsr clearscreen
	nop
	nop
	P_loadi $b0, $a000
	P_loadi $b2, $c000
	P_loadi $b4, $5555
	P_loadi $58, $d020
	P_loadi $5a, $0001
.l1:	P_store $b4, $b0
	P_add  $5a, $b0
	P_transfer $b2, $5c
	P_sub $b0, $5c
	P_branchNZ $5c, .l1
	P_sub $b0, $b2
	P_transfer  3, $fc ;Transfer R$3⇾R$fc
	P_exit
	nop
	nop
	stx $d020
	sty $d021
	sta $0400
	nop
	rts
