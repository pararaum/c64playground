;;; Example program to access basic variables.
;;; xa -I ../include/ -l /dev/stdout variable_access.asm
	* = $0801-2
	SPRITE_DATA = 960	;1024 - 64

	.word	$0801
	.byte $10, $08, $AA, $01, $9e, $20, $20, $20, $20, $20, $32, $31, $37, $35, $00
	.byte $23, $08, $AA, $02, $81, $20, $54, $b2, $30, $20, $a4, $20, $31, $30, $30, $30
	.byte $30, $30, $00, $3c, $08, $AA, $03, $58, $25, $20, $b2, $bf, $28, $54, $ad, $33
	.byte $33, $ac, $ff, $29, $ac, $33, $30, $aa, $31, $30, $30, $00, $56, $08, $AA, $04
	.byte $59, $25, $20, $b2, $20, $be, $28, $54, $ad, $33, $33, $ac, $ff, $29, $ac, $33
	.byte $30, $aa, $31, $30, $30, $00, $64, $08, $AA, $05, $99, $20, $54, $3b, $58, $25
	.byte $3b, $59, $25, $00, $74, $08, $dc, $05, $9e, $20, $20, $20, $20, $20, $34, $39
	.byte $31, $35, $32, $00, $7a, $08, $AA, $06, $82, $00, $00, $00

	.byte $0,0,0

main:	lda #<CODEPTR
	sta $fc
	lda #>CODEPTR
	sta $fd
	lda #$00
	sta $fe
	lda #$c0
	sta $ff
	lda #$10
	jsr MEMCPY
	;; Now init
	jsr init
	;; Set colours
	ldy #$0d
	sty $d020
	ldy #$05
	sty $d021
	ldy #$0B
	sty $0286		;cursor colour
	rts

#include "memcpy.inc"

	.text
CODEPTR:	
	* = $C000
code:	jmp move_sprite


init:	.(
	lda #$ff
	sta $d015		;enable sprites
	ldy #7
l2:	lda cols,y
	sta $d027,y		;sprite y colour
	dey
	bpl l2
	ldx #$07
	lda #SPRITE_DATA/64
l3:	sta $07F8,x		;set sprite pointer x
	dex
	bpl l3
	lda #$AA
	ldx #$00
l1:	sta SPRITE_DATA,x
	sta SPRITE_DATA+1,x
	sta SPRITE_DATA+2,x
	eor #$ff
	inx
	inx
	inx
	cpx #$3f
	bcc l1
	rts
cols:	.byte	1,5,15
	.byte	13,14
	.byte	12,11
	.byte	6
	.)

move_sprite:	.(
	ldx #12
l1:	lda $d000,x
	sta $d002,x
	lda $d001,x
	sta $d003,x
	dex
	dex
	bpl l1
	lda #'X'		;First character of variable name to find.
	ldx #0			;Second character, here none.
	jsr get_int_variable
	sta $0400+38
	stx $0400+39
	sta $d000
	lda #'Y'
	ldx #0
	jsr get_int_variable
	sta $d001
	sta $0400+36
	stx $0400+37
	rts
	.)

;;; Get a basic integer variable.
;;; A/X contain the two character of the variable name.
;;; Return variable value in A/X (low/high)
get_int_variable:	.(
	ora #$80		;Integer variables have the highes bit set.
	sta $45
	txa
	ora #$80		;Integer variables have the highes bit set.
	sta $46
	;; https://www.c64-wiki.com/wiki/BASIC-ROM
	;; https://skoolkid.github.io/sk6502/c64rom/asm/B08B.html
	jsr $B0E7		;Find variable
	;; Remember that variables are stored HIGH then LOW!
	ldy #$00
	lda ($47),y
	tax
	iny
	lda ($47),y
	rts
	.)
