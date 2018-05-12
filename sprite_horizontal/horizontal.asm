;;; asm6502 -e horizontal.asm -b 2047
	;; SCROLLCOUNTER = $02
SCROLLCOUNTER = $0400+39
SCRCHRBUF = $0400+40		;8 Bytes
SCRTEXTPTR = $03	

	.org $0801 - 2
	.word $0801

	.word next_basic	;Next basic line
	;; This only works as $030c to $030f are cleared on reset and
	;; $030c is moved into the status register.
	bvc main		;Line Number!
	.byte $9e		;SYS
	.byte " ( 2051 ) : ",$8F," \r\x05\"\x14\xd3HORIZONTAL SPRITE SCROLLER\xd3"
next_basic:
	.byte 0,0,0		;End of Basic Program

main:	cld
	lda #5			;White
	jsr $FFD2
	lda #147		;Clear Screen
	jsr $FFD2
	lda #120
	jsr $ffd2
	lda #97
	jsr $ffd2
	lda #115
	jsr $ffd2
	lda #122
	jsr $ffd2
	sei
	ldx #$ff
	txs
	lda #$0b
	sta $d020
	sta $d021
	lda #$35		;Turn off the BASIC and KERNAL rom.
        sta $01
	lda #<irq
	sta $fffe
	lda #>irq
	sta $ffff
	lda #$7f
        sta $dc0d               ;disable CIA interrupts
        sta $dd0d
        lda $dc0d               ;clear pending interrupts
        lda $dd0d
        lda #%00000001          ;Enable raster IRQ
        sta $d01a
	lda #60		;Raster IRQ
	sta $d012
	lda #$1b		;MSB of IRQ, normal Text
	sta $d011
	jsr init_sprite
	nop			;Init scroller
	jsr init_scroller
	cli
	inc $d020
	dec $d020
	jmp *-6

init_scroller:		lda #<scroll_text
	sta SCRTEXTPTR
	lda #>scroll_text
	sta SCRTEXTPTR+1
	lda #$00
	ldx #$07
.l1:	sta SCRCHRBUF,x
	dex
	bpl .l1
	rts

init_sprite:
	lda #$ff		;Sprites on
	sta $d015
	sta $d01d		;double width
	ldy #0
	ldx #0
	stx $ff
.l1:	lda .posx,x		;X
	sta $d000,y
	lda #229		;Y
	sta $d001,y
	lda #1			;white
	sta $d027,x
	lda .posiy,x
	ora $ff
	sta $ff
	lda .sprptr,x		;Set sprite pointer
	sta $07f8,x
	inx
	iny
	iny
	cpx #8
	bne .l1
	lda #$ff
	eor $ff
	sta $d010
	lda #00			;Clear sprite data.
	tax
.l2	sta $3e00,x
	sta $3f00,x
	dex
	bne .l2
	rts
.posx:	.byte 24,24+1*48,24+2*48,24+3*48,24+4*48,24+5*48,24+6*48,24+7*48
.posiy	.byte (24 < 256)<<0,(24+1*48 < 256)<<1,(24+2*48 < 256)<<2,(24+3*48 < 256)<<3,(24+4*48 < 256)<<4,(24+5*48 < 256)<<5,(24+6*48 < 256)<<6,(24+7*48 < 256)<<7
.sprptr: .byte $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff


irq:	dec SCROLLCOUNTER
	bpl .noscl		;no new char
	lda #9
	sta SCROLLCOUNTER
	ldx #0
	lda (SCRTEXTPTR,x)
	bne .notextend
	jsr init_scroller
	lda #' '
.notextend:
	tax
	lda tab_petscii2screencode,x
	sta $0400+38
	inc SCRTEXTPTR
	bne .nocarry
	inc SCRTEXTPTR+1
.nocarry:
	sta $fe
	lda #0
	sta $ff
	asl $fe
	rol $ff
	asl $fe
	rol $ff
	asl $fe
	rol $ff
	clc
	lda $fe
	adc #<charset
	sta .l2+1
	lda $ff
	adc #>charset
	sta .l2+2
	ldy #7
.l2	lda charset,y
	sta SCRCHRBUF,y
	dey
	bpl .l2
.noscl:	ldy #0
	ldx #0
.l1:	txa
	pha
	tya
	tax
	asl SCRCHRBUF,x
	pla
	tax
	rol $3fc2,x		;7
	rol $3fc1,x
	rol $3fc0,x
	rol $3f82,x		;6
	rol $3f81,x
	rol $3f80,x
	rol $3f42,x		;5
	rol $3f41,x
	rol $3f40,x
	rol $3f02,x		;4
	rol $3f01,x
	rol $3f00,x
	rol $3ec2,x		;3
	rol $3ec1,x
	rol $3ec0,x
	rol $3e82,x		;2
	rol $3e81,x
	rol $3e80,x
	rol $3e42,x		;1
	rol $3e41,x
	rol $3e40,x
	rol $3e02,x		;0
	rol $3e01,x
	rol $3e00,x
	txa
	clc
	adc #3
	tax
	iny
	cpy #8
	bne .l1
	asl $d019
	rti

tab_petscii2screencode:
	;; http://codebase64.org/doku.php?id=base:petscii_to_screencode
										;PETSCII RANGE
	.byte $80,$81,$82,$83,$84,$85,$86,$87, $88,$89,$8a,$8b,$8c,$8d,$8e,$8f	;$00-...
	.byte $90,$91,$92,$93,$94,$95,$96,$97, $98,$99,$9a,$9b,$9c,$9d,$9e,$9f	;...-$1f
	.byte $20,$21,$22,$23,$24,$25,$26,$27, $28,$29,$2a,$2b,$2c,$2d,$2e,$2f	;$20-...
 	.byte $30,$31,$32,$33,$34,$35,$36,$37, $38,$39,$3a,$3b,$3c,$3d,$3e,$3f	;...-$3f
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$40-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$1b,$1c,$1d,$1e,$1f	;...-$5f
	.byte $40,$41,$42,$43,$44,$45,$46,$47, $48,$49,$4a,$4b,$4c,$4d,$4e,$4f	;$60-...
 	.byte $50,$51,$52,$53,$54,$55,$56,$57, $58,$59,$5a,$5b,$5c,$5d,$5e,$5f	;...-$7f
	.byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7, $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf	;$80-...
 	.byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7, $d8,$d9,$da,$db,$dc,$dd,$de,$df	;...-$9f
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$a0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$7f	;...-$bf
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$c0-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$1b,$1c,$1d,$1e,$1f	;...-$df
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$e0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$5e	;...-$ff

scroll_text:
	.byte "@@ WELCOME TO THE HORIZONTAL SCROLLER. THIS SIMPLE SPRITE SCROLLER WAS WRITTEN IN 2018. FONT WAS RIPPED FROM KOFLERS HOMEPAGE.   "
	.byte 0

	.org $4000
charset_file:
	incbin "scrap_writer_iii_06.64c"
	incbin "scrap_writer_iii_22.64c"
	incbin "intro_studio_b.64c"
	incbin "devils_collection_16.64c"
charset = charset_file+2
