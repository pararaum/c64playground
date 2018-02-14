	* = $0801-2

	SCREENPTR = $CC00
	BITMAPPTR = $E000
	
	.bin 0,0,"basic_header_with_sys_2304.bin"
	
	.dsb 2304-*, $EA
	jmp begin

gfxdata	.bin 0,0,"gfx.c64"
siddata .bin 0,0,"mzk.sid"
sidend

	;; Copy from ($fc) to ($fe), number of pages in A
cpymem:	pha
	txa
	pha
	tya
	pha
	tsx
	lda $0103,x		;Get old accumulator value from stack
	tax			;X contains number of pages
	ldy #$00
cpy1:	lda ($fc),y
	sta ($fe),y
	iny
	bne cpy1
	inc $fd
	inc $ff
	stx $d020
	dex
	bne cpy1
	pla
	tay
	pla
	tax
	pla
	rts

	.aasc "pararaum"

stackx:	nop
	tsx
	lda !$0101,x
	sta $02
	lda !$0102,x
	sta $03
	pla
	sta $04
	pla
	sta $05
	ldy #1
	clv
l3	lda ($02),y
	beq s_end
	sta $03FF,y
	lda ($04),y
	beq s_end
	sta $0427,y
	iny
	jmp l3
s_end:	lda #0
	sta $d020
	tya
	clc
	adc $02
	sta $06
	lda #$00
	adc $03
	sta $07
	pha
	lda $06
	pha
	;; 	bvc *
	rts
	
	.byte 2
	lda $f0,x
	lda ($c0),y
	.byte 2	
	rts
	
begin:	nop
	jsr stackx
	.asc "PARARAUM",0
	nop
	nop
	sei
	
	;; GfX Init
	jsr gfxinit

	;; Copy color information
	lda #<(gfxdata+2+8000)
	sta $fc
	lda #>(gfxdata+2+8000)
	sta $fd
	lda #<SCREENPTR
	sta $fe
	lda #>SCREENPTR
	sta $ff
	lda #$04
	jsr cpymem
	;; Copy bitmap
	lda #<gfxdata+2
	sta $fc
	lda #>gfxdata+2
	sta $fd
	lda #<BITMAPPTR
	sta $fe
	lda #>BITMAPPTR
	sta $ff
	lda #$20
	jsr cpymem
	;; Copy Muzak
	lda #<siddata+$7e
	sta $fc
	lda #>siddata+$7e
	sta $fd
	lda #<$0810
	sta $fe
	lda #>$0810
	sta $ff
	lda #(sidend-siddata)/256+1
	jsr cpymem

	;; Initialise sprites
	jsr spriteinit
	lda #$00
	sta BITMAPPTR+$1FFF
	
	lda #$02		;initialise music
	jsr $0810
	
l1	lda #$01		;border white
	sta $d020
	bit $d011		;wait until top-most bit of raster line is cleared
	bmi l1
	ldy #%00111011		;Normal value for 
	sty $d011
	inc $d020

	lda #$F8		;Wait until beginning of border
	ldy #%00110011
l139:	cmp $d012
	bne l139
	sty $d011		;Switch border off
	inc $d020
	
l2:	bit $d011
	bpl l2
	inc $d020
	jsr $0941		;play music
	inc $d020
	lda #$10
l153:	cmp $d012
	bcc l153
	inc $d020
	jmp l1

gfxinit	lda #(((SCREENPTR & $3FFF)/$0400) << 4) | (((BITMAPPTR & $3FFF)/$0800) << 1)
	sta $d018
	lda #%00111011		; Hires
	sta $d011
	lda $dd00
	and #%11111100
	ora #(BITMAPPTR ^ $FFFF) >> 14
	sta $dd00
	rts
	
spriteinit:
	ldx #<sprite_positions
	ldy #>sprite_positions
	jsr set_sprite_positions
	lda #$ff		;activate sprites
	sta $d015
	lda #$01
	ldy #8
l178:	sta $d027,y
	sta SCREENPTR+$3f8,y
	dey
	bpl l178
	rts

sprite_positions:
        .byt    %00000000, $18, $0
        .byt    %00000000, $18+24, $0
        .byt    %00000000, $18+24*2, $0
        .byt    %00000000, $18+24*3, $0
        .byt    %00000000, $18+24*4, $0
        .byt    %00000000, $18+24*5, $0
        .byt    %00000000, $18+24*6, $0
        .byt    %00000000, $18+24*7, $0
	
	;; Set sprite positions accordings to table
        ;; I pointer to sprite positions (X=low, Y=high)
        ;; O
        ;; D A,X,Y
set_sprite_positions:   .(
	stx get_next+1
	sty get_next+2
        ldx #0
        ldy #0
        lda #0
        sta v189
l197:   lsr v189
	jsr get_next
l192:   ora v189
        sta v189
        inx
	jsr get_next
        sta $d000,y
        inx
        iny
	jsr get_next
        sta $d000,y
        inx
        iny
        cpy #16
        bcc l197
        lda v189
        sta $d010
        rts
get_next:
        lda $FFFF,x
	rts	
v189:   .byte 0
        .)
        
