;;; xa border.asm
;;; xa -l /dev/stderr border.asm

	CHROUT = $FFD2
	frame_counter = $2	;4 bytes
	irq_counter = $6	;2 bytes
	raster_increment = 4	;Next irq is this many lines later.
	sprite_data = $200	;Where is the sprite data stored?

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
	;; Repair chrout vector.
	lda #<$f1ca
	sta $0326
	lda #>$f1ca
	sta $0327
	lda #$90		;black. (format "%x" 144)"90"
	jsr CHROUT
	lda #$93		;Clear screen. (format "%x" 147)"93"
	jsr CHROUT
	sei
	lda #$35   ;Turn off the BASIC and KERNAL ROM.
        sta $01
	ldx #$ff
	txs			;Reset stack
	lda #$c			;Background medium grey.
	sta $d021
	lda #$f			;Border light grey.
	sta $d020
	jsr prepare_irq
	ldx #$fd		;Clear zeropage.
	lda #0
l1:	sta $02,x
	dex
	bne l1
	cli
	jsr setup_sprite
	jsr kool
	clv
	bvc *
	;; (format "%x" 53272)
	.)

kool:	.(
l1:	bit $d011
	bmi l1
	inc $d020
	lda #$00
	sta irq_counter
	sta irq_counter+1
l2:	bit $d011
	bpl l2
	dec $d020
	jsr output
	inc frame_counter+0
	bne l3
	inc frame_counter+1
	bne l3
	inc frame_counter+2
	bne l3
	inc frame_counter+3
l3:	nop
	lda frame_counter+0
	asl
	sta $fe
	lda frame_counter+1
	rol
	sta $ff
	lda $fe
	ldx #1
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	ror $ff
	ror $fe
	jsr sety
	jmp kool
sety:	lda $fe
	and #$7f
	clc
	adc #$e5-$7f
	sta $d000,x
	inx
	inx
	rts
	.)

SKIP:	.dsb $0800-*, 218

output:	.(
	ldy #39-8
	lda frame_counter+3
	jsr outputHex
	lda frame_counter+2
	jsr outputHex
	lda frame_counter+1
	jsr outputHex
	lda frame_counter+0
	jsr outputHex
	ldy #39-8-2-4
	lda irq_counter+1
	jsr outputHex
	lda irq_counter+0
	jsr outputHex
	rts
	.)

;;; in A val, Y pos
;;; out Y pos+2
outputHex:	.(
	pha
	lsr
	lsr
	lsr
	lsr
	jsr out
	iny
	pla
	and #$0f
	jsr out
	iny
	rts
out:	tax
	lda char,x
	sta $0400,y
	rts
char:	.asc "0123456789", 1, 2, 3, 4, 5, 6, 7
	.)


prepare_irq:	.(
	lda #$00
        sta $d012
	lda #$7f
	and $d011
	sta $d011
        lda #$7f
        sta $dc0d     ; disable timer interrupts
        sta $dd0d
        lda #%00000001          ;enable raster IRQ
        sta $d01a     ; enable raster interrupt
        lda $dc0d     ; acknowledge CIA interrupts
        lsr $d019     ; and video interrupts
	lda #<irq     ;Set new interrupt vector.
	sta $fffe
	lda #>irq
	sta $ffff
	rts
	.)

irq:	.(
	inc $d021
	pha
	inc irq_counter
	bne l1
	inc irq_counter+1
l1:	lda $d012
	clc
	adc #raster_increment
	sta $d012
	lsr $d019
	pla
	dec $d021
	rti
	.)

setup_sprite:	.(
	lda #sprite_data/64		;Position of sprite data.
	ldx #7
l1:	sta $07f8,x		;Set all sprites pointer to the same data.
	dex
	bpl l1
	ldx #63			;Copy sprite data.
l2:	lda spritebox,x
	sta sprite_data,x
	dex
	bpl l2
	ldx #16
l3:	lda pos,x
	sta $d000,x
	dex
	bpl l3
	lda #3			;Colour is cyan.
	ldx #7
l4:	sta $d027,x
	dex
	bpl l4
	lda #$ff		;Turn all sprites on.
	sta $d015
	rts

pos:	.byte $50,$e5
	.byte $60,$e5
	.byte $70,$e5
	.byte $80,$e5
	.byte $90,$e5
	.byte $a0,$e5
	.byte $b0,$e5
	.byte $c0,$e5
	.byte 0			;MSB of positions
	
spritebox:
        .byt    $FF,$FF,$FF
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$aa,$01
        .byt    $80,$55,$01
        .byt    $80,$aa,$01
        .byt    $80,$55,$01
        .byt    $80,$aa,$01
        .byt    $80,$55,$01
        .byt    $80,$aa,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $80,$00,$01
        .byt    $FF,$FF,$FF
	.)

END_OF_WORLD:	
