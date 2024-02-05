
	* = $0801 - 2

	smoothctr = $02
	vpos = $04
	rasterpos = $40
	rasterposval = $05
	scrollline = $0400 + 5*40
	scrollctr = $03
	
	.word $0801
	;; basic stub
	.byt $17, $08, $de, $07, $8f, $20, $53, $49, $4d, $50, $4c, $45, $20, $53, $43
	.byt $52, $4f, $4c, $4c, $45, $52, $00, $26, $08, $34, $08, $97, $20, $35, $33, $32
	.byt $38, $30, $2c, $31, $33, $00, $35, $08, $98, $08, $97, $20, $35, $33, $32, $38
	.byt $31, $2c, $31, $33, $00, $56, $08, $fc, $08, $8f, $20, $53, $4f, $4d, $45, $20
	.byt $43, $4f, $44, $45, $20, $57, $41, $53, $20, $52, $49, $50, $50, $45, $44, $20
	.byt $46, $52, $4f, $4d, $3a, $00, $9f, $08, $60, $09, $8f, $20, $48, $54, $54, $50
	.byt $3a, $2f, $2f, $43, $4f, $44, $45, $42, $41, $53, $45, $36, $34, $2e, $4f, $52
	.byt $47, $2f, $44, $4f, $4b, $55, $2e, $50, $48, $50, $3f, $49, $44, $3d, $42, $41
	.byt $53, $45, $3a, $49, $4e, $54, $52, $4f, $44, $55, $43, $54, $49, $4f, $4e, $5f
	.byt $54, $4f, $5f, $52, $41, $53, $54, $45, $52, $5f, $49, $52, $51, $53, $00, $c0
	.byt $08, $c4, $09, $99, $20, $22, $93, $53, $49, $4d, $50, $4c, $45, $20, $53, $43
	.byt $52, $4f, $4c, $4c, $45, $52, $20, $45, $58, $41, $4d, $50, $4c, $45, $22, $00
	.byt $cb, $08, $be, $a1, $9e, $20, $32, $33, $30, $34, $00, $00, $00, $ff, $ff, $ff
	.byt $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byt $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byt $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

	lda #$0d
	sta $d020
	sta $d021
	lda #5
	ldx #0
l28	sta $d800,x
	sta $d900,x
	sta $da00,x
	sta $db00,x
	inx
	bne l28
	
	sei        ;disable maskable IRQs
	lda #$7f
	sta $dc0d  ;disable timer interrupts which can be generated by the two CIA chips
	sta $dd0d  ;the kernal uses such an interrupt to flash the cursor and scan the keyboard,
		   ;so we better stop it.
	lda $dc0d  ;by reading this two registers we negate any pending CIA irqs.
	lda $dd0d  ;if we don't do this, a pending CIA irq might occur after we finish setting up our irq.
           ;we don't want that to happen.
	lda #$01   ;this is how to tell the VICII to generate a raster interrupt
	sta $d01a

	lda #rasterpos   ;this is how to tell at which rasterline we want the irq to be triggered
	sta rasterposval
	sta $d012
	lda #$1b   ;as there are more than 256 rasterlines, the topmost bit of $d011 serves as
	sta $d011  ;the 8th bit for the rasterline we want our irq to be triggered.
			;here we simply set up a character screen, leaving the topmost bit 0.

	lda #$35   ;we turn off the BASIC and KERNAL rom here
	sta $01    ;the cpu now sees RAM everywhere except at $d000-$e000, where still the registers of
           ;SID/VICII/etc are visible

	lda #<irq  ;this is how we set up
	sta $fffe  ;the address of our interrupt code
	lda #>irq
	sta $ffff
	jsr init
	lda #$00
	jsr $fff		;Initialise song
	cli	;enable maskable interrupts again
waitbut	lda #%00010000
        bit $dc00		;Firebutton pressed?
	bne waitbut
	sei
	lda #$37
	sta $01
	brk

init	lda #0
	sta scrollctr
	sta vpos
	lda #1
	sta smoothctr
	lda #$00		;Remove black from bad lines
	sta $3fff
	rts
	
irq	pha
	jsr fld
 	jsr scroll
	ldy #$1b
l107	lda #$a5
	cmp $d012
	bne *-3
	sty $d011
	lda #$c8
	sta $d016
	jsr $1003		;Play music
	lda #$ff   ;this is the orthodox and safe way of clearing the interrupt condition of the VICII.
	sta $d019  ;if you don't do this the interrupt condition will be present all the time and you end
		;up having the CPU running the interrupt code all the time, as when it exists the	
		;interrupt, the interrupt request from the VICII will be there again regardless of the
	        ;rasterline counter.
	pla
	rti

fld:	ldy vpos
	ldx cosvpos,y
	iny
	cpy #cosvposend-cosvpos
	bne l106
	ldy #00
l106:	sty vpos
	cpx #0
	bne l129
	rts
l129	lda $d012
	cmp $d012
	beq *-3
	clc
	lda $d011
	adc #1
	and #7
	ora #$18
	sta $d011
	dex
	bne l129
	rts
	
scroll	ldx smoothctr
	dex
	bpl l108
	jsr scrollonechar
	ldx #((7+1)<<1)-1
l108	stx smoothctr
	txa
	lsr
	and #$07
	sta $d016
	rts
scrollonechar:
	ldy #0
l112	lda scrollline+1,y
	sta scrollline,y
	iny
	cpy #39
	bne l112
	ldy scrollctr
	lda scrolltxt,y
	and #$3f		;primitive PETSCII to screen code
	sta scrollline+39
	iny
	cpy #scrolltxtend-scrolltxt
	bne l125
	ldy #$00
l125:	sty scrollctr
	rts

scrolltxt:	.asc  "THIS IS SOME WONDERFUL TEXT FOR MY SMOOTH SCROLLER. THE CODE IS STILL A LITTLE BIT CRAPPY BUT THIS WILL IMPROVE... "
	.asc "PARARAUM OF T7D... "
	.asc "GREETINGS GO TO MEEPSTER, JACK BEATMASTER, ROZ, STROBO, ZAKE, PAUL HOLT, TEZ, ET AL."
        .asc "                   "
scrolltxtend:

cosvpos:
	;; 	.byt 	7, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7
	;; 	.byt 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 3
	.byt 	30, 33, 36, 39, 42, 45, 47, 50, 52, 54, 55, 57, 58, 59, 59, 60, 59, 59, 58, 57, 55, 54, 52, 50, 47, 45, 42, 39, 36, 33, 30, 26, 23, 20, 17, 14, 12, 9, 7, 5, 4, 2, 1, 0, 0, 0, 0, 0, 1, 2, 4, 5, 7, 9, 12, 14, 17, 20, 23, 26
cosvposend:
	
	.dsb	($fff-*-$7c-2), $D2
	.bin	0,0,"I_Died_Defending_the_Mothership.sid"