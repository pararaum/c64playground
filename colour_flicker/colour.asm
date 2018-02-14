	SCREENPTR = $CC00
	BITMAPPTR = $E000
	
	.word $326
	* = $326	;DO NOT CHANGE, else the autostart will fail
	.word boot	;autostart from charout vector ($f1ca)
	.word $f6ed	;$328 kernal stop routine Vector ($f6ed)
	.word $f13e	;$32a kernal getin routine ($f13e)
	.word $f32f	;$32c kernal clall routine vector ($f32f)
	.word $fe66	;$32e user-defined vector ($fe66)
	.word $f4a5	;$330 kernal load routine ($f4a5)
	.word $f5ed	;$332 kernal save routine ($f5ed)

;* = $334 (cassette buffer)

boot:	.(
	sei
	lda #$ca	;repair charout vector ($f1ca)
	sta $326
	lda #$f1
	sta $327
	jsr gfxinit
	jsr fillscr
	jsr fillclr
l24:	jsr waitvr
	jsr eorclr
	jmp l24
	jmp *
	;; (format "%x" 53272)
	.)

eorclr:	.(
	lda SCREENPTR
	eor #$22
	ldx #0
l1	sta SCREENPTR+$0000,x
	sta SCREENPTR+$0100,x
	sta SCREENPTR+$0200,x
	sta SCREENPTR+$0300,x
	dex
	bne l1
	rts
	.)

fillclr:	.(
	lda #$46
	ldx #0
l1	sta SCREENPTR+$0000,x
	sta SCREENPTR+$0100,x
	sta SCREENPTR+$0200,x
	sta SCREENPTR+$0300,x
	dex
	bne l1
	rts
	.)

gfxinit:	pha
	lda #(((SCREENPTR & $3FFF)/$0400) << 4) | (((BITMAPPTR & $3FFF)/$0800) << 1)
	sta $d018
	lda #%00111011		; Hires
	sta $d011
	lda $dd00
	and #%11111100
	ora #(BITMAPPTR ^ $FFFF) >> 14
	sta $dd00
	pla
	rts

fillscr:	.(
	ldx #$20
	lda #>BITMAPPTR
	sta $ff
	lda #<BITMAPPTR
	sta $fe
	lda #$AA
	sta $fd
l70:	ldy #0
l71:	sta ($fe),y
	eor #$ff
	dey
	bne l71
	inc $ff
	dex
	bne l70
	rts
	.)
	
waitvr:	lda #$04
	bit $d011
	bmi *-3
	sta $d020
	lda #$06
	bit $d011
	bpl *-3
	sta $d020
	rts
END:	NOP
