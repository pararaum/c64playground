	SPRITE_COLOR = $06	;Blue
	SCREEN_FILL_CHAR = $E6
	IRQ_DISPATCH_ZP = 10
	toprasterline = 51
	bottomrasterline = 248
	storea = 7
	storex = 3
	storey = 4
	sprite_copyptr = 5		;two bytes
	scroll_counter = 2
	scrolltext_ptr = 8	;pointer
	TEMP = $F0		;16 temporary "registers"

	.word $0801
	* = $0801
	.bin 0,0,"basic_stub.bin"
	.dsb $0834 - *, $EA

	lda #$00
	sta scroll_counter
	jsr clear_screen
	jsr do_the_gfx
	jsr init
inft:	inc $0400+40*25-1
	bne inft
	inc $0400+40*25-2
	bne inft
	inc $0400+40*25-3
	bne inft
	inc $0400+40*25-4	
	jmp inft

do_the_gfx:	.(
	lda $d018
	pha
	and #$F0
	ora #%00001000
	sta $d018
	lda $d011
	pha
	ora #%00100000
	sta $d011
	ldy #$C
	ldx #$00
l1:	lda $d012
	sta $d020
	bne l1
	lda #%00010000
	and $DC00
	beq out
	dex
	bne l1
	dey
	bne l1
out:	pla
	sta $d011
	pla
	sta $d018
	rts
	.)

init:	.(
	lda #<scroll_text	;Initialise the scroll text
	sta scrolltext_ptr
	lda #>scroll_text
	sta scrolltext_ptr+1
	nop
	ldx #$00
	stx IRQ_DISPATCH_ZP	;Clear interrupt dispatch
	jsr irq_dispatcher_copy
	stx IRQ_DISPATCH_ZP	;Clear interrupt dispatch
	nop
	lda #$3
	sta $d020
	lda #$d
	sta $d021
	lda #$ff
	sta $d01d		;sprites double width
	sta $d017		;sprites double height
	lda #%11111111
	sta $d015		;turn them on (a bit for every sprite)
	lda #%00000000		;No Sprite has high bit set!
	sta $d010
	ldx #7
	lda #SPRITE_COLOR
l59:	sta $d027,x 		;sprite colour
	;; 	eor #SPRITE_COLOR
	dex
	bpl l59
	ldx #0
l1	lda sprite_positions,x
	sta $d000,x
	inx
	cpx #$10
	bne l1
	ldx #0
	stx $3fff		;remove black
	ldy #sprite_data/$40	;initialise sprite pointer value
l2	lda #5
l2_a	txa
	sta $0400+30,x
	tya
	sta $07f8,x		;sprite-data pointer
	iny
	inx
	cpx #8
	bne l2_a
	sei                     ;disable interrupts
	lda #1
	sta $d01a		;interrupt request (IRQ on raster line)
	lda #toprasterline
	sta $d012
        lda #$7f
        sta $dc0d               ;disable CIA interrupts
        sta $dd0d
        lda $dc0d               ;clear pending interrupts
        lda $dd0d
        lda #<irq_dispatcher	;this is how we set up
        sta $fffe		;the address of our interrupt code
        lda #>irq_dispatcher
        sta $ffff
	lda #$35   ;we turn off the BASIC and KERNAL rom here
        sta $01
        cli
	rts
	.)
;;;
next_char:	.(
	pha
	sty TEMP
	ldy #0
	lda (scrolltext_ptr),y
	bne noend
	lda #<scroll_text
	sta scrolltext_ptr
	lda #>scroll_text
	sta scrolltext_ptr+1
	lda #' '
noend:	ldy TEMP
	jsr copy_char
	inc scrolltext_ptr
	bne noover
	inc scrolltext_ptr
noover:	pla
	rts
	.)
	
;;; Next char has to be in A. X/Y (L/H) contains the position to copy
;;; the char to.
copy_char:	.(
	stx modi+1
	sty modi+2
	pha
	lda #$00
	sta TEMP+1		;clear high byte pointer
	pla
	tax
	lda tab_petscii2screencode,x
	asl			;Now multiply by eight.
	rol TEMP+1
	asl
	rol TEMP+1
	asl
	rol TEMP+1
	sta TEMP
	lda TEMP
	clc
	adc #<chrset
	sta TEMP
	lda TEMP+1
	adc #>chrset
	sta TEMP+1
	ldy #7
	ldx #7*3
l121:	lda (TEMP),y
modi:	sta $EAEA,x		;Self modification
	dex
	dex
	dex
	dey
	bpl l121
	rts
tab_petscii2screencode:		;Ripped from Codebase64.
										;PETSCII
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
	.)

irq_dispatcher:
	;; We ignore BRK, remember that!
	sta storea
	stx storex
	sty storey
	tsx
	lda $0101,x
	and #$10
	beq irq_dispatcher_vector
	.byt $22
	;; dispatch
irq_dispatcher_vector:	jsr irq_dispatcher_vector
	.(
	;; Now update for the next.
	ldx IRQ_DISPATCH_ZP
	lda irq_table+1,x	;If high byte is zero then End Of List
	bne noreset
	ldx #$00
noreset:
	jsr irq_dispatcher_copy
	stx IRQ_DISPATCH_ZP
	lda #$ff
        sta $d019               ;acknowledge IRQ
	lda storea
	ldx storex
	ldy storey
	rti
	.)
;;; Copy routine for IRQ dispatcher
;;; 
irq_dispatcher_copy:	.(
	lda irq_table,x
	sta irq_dispatcher_vector+1
	inx
	lda irq_table,x
	sta irq_dispatcher_vector+2
	inx
	lda irq_table,x
	sta $d012
	inx
	lda irq_table,x
	bne highbit
	lda $d011
	and #%01111111
	sta $d011
	jmp cont
highbit:	lda $d011
	ora #%10000000
	sta $d011
cont:	inx
	rts
	.)
	
irq:	.(
	lda #$1b
	sta $d011		;25 lines
	lda #$01
	sta $d020		;Border set to white.
	jsr scroll_up_six
	dec scroll_counter
	bpl nochr
	ldx #7
	stx scroll_counter
	ldx #<(sprite_data+6*64+12*3)
	ldy #>(sprite_data+6*64+12*3)
	jsr next_char
nochr:	lda #3
	sta $d020
	rts
	.)

irq2:	.(
	lda #$13
	sta $d011		;24 lines
	lda #$00
	sta $d020
	lda $d012
l1:	cmp $d012
	beq l1
	lda #$03
	sta $d020
	rts
	.)

irq3:	.(
	lda #$04
	sta $d020
	lda #%11111110
	sta $d015		;turn off sprite 0
	lda $d012
l1:	cmp $d012
	beq l1
	lda #$03
	sta $d020
	rts
	.)

irq4:	.(
	lda #$02
	sta $d020
	lda #%10111111
	sta $d015		;turn off sprite 6
	lda $d012
l1:	cmp $d012
	beq l1
	lda #$03
	sta $d020
	rts
	.)

scroll_up_six:	.(
	ldx #$00
	stx scr_reg
l1:	pha
	ldy scr_reg
	lda ptr,y
	tax
	iny
	lda ptr,y
	iny
	sty scr_reg
	tay
	pla
	jsr scroll_up
	ldx scr_reg
	cpx #2*7
	bne l1
	rts
	
ptr:	.word sprite_data+6*64
	.word sprite_data+5*64
	.word sprite_data+4*64
	.word sprite_data+3*64
	.word sprite_data+2*64
	.word sprite_data+1*64
	.word sprite_data+0*64
scr_reg:	.dsb 1
	.)

;;;Scroll up one line.
;;;
;;;This function will scroll up the leftmost eight pixel of a sprite
;;;one line up.
;;;A new value to put up in the bottom
;;;X/Y pointer to sprite data (L/H)
;;;returns A top eight pixels

scroll_up:	.(
	stx sprite_copyptr
	sty sprite_copyptr+1
	tax			;save pixel value to X
	ldy #0
	lda (sprite_copyptr),y
	pha			;store top row
	ldy #03
l1:	lda (sprite_copyptr),y
	dey
	dey
	dey
	sta (sprite_copyptr),y
	tya
	clc
	adc #6
	cmp #63
	bcs out
	tay
	jmp l1
out:	txa			;save value to be put into bottom
	ldy #60
	sta (sprite_copyptr),y
	pla			;retrieve top row
	rts
	.)

clear_screen:	.(
	ldx #$00
l1:	lda #SCREEN_FILL_CHAR
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	lda #5			;green
	sta $d800,x
	sta $d900,x
	sta $da00,x
	sta $db00,x
	inx
	bne l1
	rts
	.)

END_OF_CODE:
	.dsb ((* | $ff) - *) + 1, $FF
scroll_text:	.asc " HELLO AND WELCOME", $5e
	.asc " APPARENTLY, THERE ARE NO EXCLAMATION MARKS?"
	.asc " THIS LITTLE SCROLLER WAS WRITTEN WHILE VISITING EVOKE 2017."
	.ASC " THIS IS THE 20TH EVOKE. THE MUSIC IS FAR TOO LOUD BUT THE CROWD IS SUPERP... "
	.asc " @@@ ", 0
irq_table:
	.word irq, toprasterline
	.word irq2, bottomrasterline
	.word irq3, $101
	.word irq4, $11e
	.word 0,0
sprite_positions:
	.byte 30,15
	.byte 30,15+1*21*2
	.byte 30,15+2*21*2
	.byte 30,15+3*21*2
	.byte 30,15+4*21*2
	.byte 30,15+5*21*2
	.byte 30,11		;This is the final sprite which gets the new data.
	.byte 130,15

	.dsb $2000-*
chrset:	.bin 2,0,"beyond_the_ice_palace.64c"
;;; (format "$%04X" (/ 16384 64))"$0100"
sprite_data_SKIP:
	.dsb 16384-8*64-*,$00
sprite_data:
	.dsb 8*64,$00
END_OF_DATA
