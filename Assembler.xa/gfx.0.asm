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

boot	sei
	lda #$ca	;repair charout vector ($f1ca)
	sta $326
	lda #$f1
	sta $327
	lda $d011
	ora #$20
	sta $d011
	lda $d018
	ora #8
	sta $d018
	lda #$76
	ldx #0
l1	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	dex
	bne l1
	ldy #00
	sty $fb
	lda #$20
	sta $fc
	ldx #$20
	lda #1
l2	sta ($fb),y
	rol
	iny
	bne l2
	inc $fc
	dex
	bne l2
	jsr openb
	jmp *
	;; (format "%x" 53272)

openb:	bit $d011
	bpl *-3
	bit $d011
	bmi *-3
	lda #$3b
	sta $d011

	;; For each frame, set screen-mode to 24 lines at y-position $f9 - $fa..
loop:
	lda #$f9
	cmp $d012
	bne *-3
	lda $d011
	and #$f7
	sta $d011

	;; .. and below y-position $fc, set it back to 25 lines.
	bit $d011
	bpl *-3
	ora #8
	sta $d011
	inc $3fff		;works only in text mode?
	jmp loop
