;;; RAM expansion hack.
;; See https://github.com/svn2github/vice-emu/blob/master/vice/src/c64/plus60k.c
;;; x64 -memoryexphack 2 -virtualdev +truedrive +warp -iecdevice8 -fs8 . -8 .

RAMHACK = $D100

	* = $801-2

	.word $0801
	.word eobas		;End Of Basic
	.word $babe
	.byte $9e		;SYS
	.asc "2061"
eobas:	.byte 0,0,0

main:	lda #$81
	ldx #$80
	ldy #$00
	sta $cfff
	stx RAMHACK
	lda $cfff
	sta $0400
	lda #$18
	sta $cfff
	sty RAMHACK
	lda $cfff
	sta $0401
	rts
