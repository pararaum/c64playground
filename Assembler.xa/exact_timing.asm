;;; Some exact timing test with border removal, etc.

	* = $326-2	;DO NOT CHANGE, else the autostart will fail

	rasterline = $37+22
	spritex = 0

	.word $326
	.word boot	;autostart from charout vector ($f1ca)
	;; You need these otherwise you'll jam.
        .word $f6ed     ;$328 kernal stop routine Vector ($f6ed)
        .word $f13e     ;$32a kernal getin routine ($f13e)
        .word $f32f     ;$32c kernal clall routine vector ($f32f)
        .word $fe66     ;$32e user-defined vector ($fe66)
        .word $f4a5     ;$330 kernal load routine ($f4a5)
        .word $f5ed     ;$332 kernal save routine ($f5ed)

boot:	sei			;disbale interrupts
	lda #$7f
	sta $dc0d		;disable CIA interrupts
	sta $dd0d
	lda $dc0d		;clear pending interrupts
	lda $dd0d
	
	lda #1
	sta $d01a		;raster IRQ
	lda #rasterline
	sta $d012
	lda #$1b
	sta $d011		;rasterline msb
	lda #$ff		;switch all sprites on
	sta $d015
	lda #rasterline-3
	sta $d001
	clc
	adc #21
	sta $d003
	lda #spritex
	sta $d000
	sta $d002
	lda #0
	sta $3fff
	
	lda #$35   ;we turn off the BASIC and KERNAL rom here
	sta $01
	lda #<irq  ;this is how we set up
	sta $fffe  ;the address of our interrupt code
	lda #>irq
	sta $ffff
	cli

alw2:	inc $3fff ;$0400+39
	jmp alw2
always	lda #$A0
	cmp $d012
	bne *-3
	lda #$3
	sta $d021
	lda #$D0
	cmp $d012
	bne *-3
	lda #$4
	sta $d021
	lda #$08
	sta $d016		;40 chars/line
	jmp always

irq	ldx #$4			;2
	dex			;2
	bne *-1			;2/3
	ldx #35		;2
	inc $d021
	dec $d021
;;;  	BIT $EA			;3
lineloop:	
	nop			;2
	nop			;2
	dec $d016		;(6) 38 chars/line
	inc $d016		;6
	LDY $D012		;4
	DEY			;2
	NOP			;2
	TYA			;2
	AND #$07		;2
	ORA #$18		;2
	STA $d011		;4

	cpy #rasterline+18		;2
	bcc ahop		;2/3

	dec $d021		;6
	inc $d021		;6
	jmp ahop2
ahop:	nop			;2
	nop			;2
	inc $d021		;6
	dec $d021		;6
ahop2:	DEX			;2
	BPL lineloop		;2/3
quitloop:
	LDA #$1B
	STA $D011
	lda #$ff		;2
	sta $d019		;acknowledge IRQ (4)
	rti
ENDOFCODE
;;; Some code ripped from http://stackoverflow.com/questions/1477444/how-do-i-show-sprites-in-the-border-on-c64
;;; See also: http://www.antimon.org/dl/c64/code/missing.txt
