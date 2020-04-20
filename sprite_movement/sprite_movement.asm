#define push16(x) lda #>(x):pha:lda #<(x):pha
#define load16(r,x) lda #>(x):sta r+1:lda #<(x):sta r
#define setzeropage(r,x) lda #<(x):sta r:lda #>(x):sta r+1

        * = $0801-2

        SCREENPTR = $0000
        BITMAPPTR = $2000
	interrupt_ptr = $FE
	end_of_program = $02
	hex_output_pointer = $FC
	SPRITE_POINTERS = $07F8
	SID_INIT = $1000
	SID_PLAY = $1003
	
        .bin 0,0,"basic_stub.bin"
        
        .dsb 10*256-*, $00

        jmp main
	jmp checker_screen

	.asc "Pararaum of T7D"
	.dsb $1000-$7c-2-*, $00
	.bin 0,0,"Keeper.sid"
	
main:	sei
	jsr init_gfx
	lda #$00
	jsr fill_colour_ram
	lda #9			;Set foreground colour for printing
	sta $0286
	jsr init_sprite
	lda #$00
	jsr SID_INIT
 	jsr init_interrupt
        cli	;enable maskable interrupts again
	lda #$00
	sta end_of_program
l50:	jsr output_position
	bit end_of_program
	bvc l50			;endless loop
	inc $d021
	lda #$ff
l58:	jsr fill_colour_ram
l55:	bit $d012
	bvs l55
	sec
	sbc #1
	bcs l58
	brk

output_position:
	ldx #5			;Row
	ldy #$19		;Column
	clc			;Set cursor position
	jsr $FFF0
	ldy $d001
	lda #$00
	jsr print_val
	ldx #5			;Row
	ldy #$10		;Column
	clc			;Set cursor position
	jsr $FFF0
	ldy $d000
	lda $d010		;MSB of all sprites
	and #$01		;We are interested in sprite zero
	jsr print_val
	ldx #7			;Row
	ldy #$10		;Column
	clc			;Set cursor position
	jsr $FFF0
	lda #' '		;Output space and '$'
	jsr $ffd2
	lda #'$'
	jsr $ffd2
	ldy $d000
	lda $d010		;MSB of all sprites
	and #$01		;We are interested in sprite zero
	jsr int2hex
	jsr outstr_0100
	ldx #7			;Row
	ldy #$19		;Column
	clc			;Set cursor position
	jsr $FFF0
	lda #' '		;Output space and '$'
	jsr $ffd2
	lda #'$'
	jsr $ffd2
	ldy $d001		;Y-position
	lda #$00
	jsr int2hex
	jsr outstr_0100
	rts

print_val:	
	jsr $b391		;Convert A(Hi)/Y(low) to float
	jsr $BDDD		;Convert FAC to string at $0100
	jsr outstr_0100
l81	lda #$20		;X contains printed charactes, now fill to six characters
	jsr $ffd2
	inx
	cpx #6
	bcc l81
	rts
	
	;; Output a null-terminated string @ $0100.
	;; I
	;; O X=number of printed characters
	;; D A,X
outstr_0100:	.(
	ldx #0
l71	lda $0100,x
	beq l106
	jsr $ffd2		;CHROUT
	inx
	bcc l71
l106	rts
	.)

int2hex:
	pha
	load16(hex_output_pointer,$0100)
	pla
	jsr accu2hex
	load16(hex_output_pointer,$0102)
	tya
	jsr accu2hex
	ldy #$00
	sty $0104
	rts

	;; Convert A to hex string (PETSCII)
	;; I A=value to convert
	;; O
	;; D
	;; Z hex_output_pointer
accu2hex:	.(
	pha
	txa
	pha
	tya
	pha
	tsx
	lda $0103,x		;Load value to convert
	lsr
	lsr
	lsr
	lsr
	and #$0F
	tax
	lda hexchars,x
	ldx #$00
	sta (hex_output_pointer,x)
	tsx
	lda $0103,x
	and #$0f
	tax
	lda hexchars,x
	ldy #1
	sta (hex_output_pointer),y
	pla
	tay
	pla
	tax
	pla
	rts
hexchars:
	.asc "0123456789ABCDEF"
	.)


	
init_interrupt:
	jsr interrupt_setup
	lda #00
	sta irqidx
	jsr set_next_irq
	rts

init_gfx:
	ldx #$00
	stx $3fff		;black border suppression
        lda #$1b   	;as there are more than 256 rasterlines, the topmost bit of $d011 serves as
        sta $d011  	;the 8th bit for the rasterline we want our irq to be triggered.
                        ;here we simply set up a character screen, leaving the topmost bit 0.
	lda #$01
	sta $d021
	lda #$0f
	sta $d020
	rts
	
	sei
; 	lda #$81
; 	jsr $bc3c
; 	ldy #$81
; 	jsr $b3a2
; 	jsr $bddd
; 	jsr $ab1e
; 	rts
; ; 	ldx #$FF
; 	txs			;kill old stack
; 	lda #$ff
; 	sta $0100,x
; 	dex
; 	bne *-7
	jsr interrupt_setup
	lda #$00
	jsr fill_colour_ram
; 	lda #$00
; 	jsr txtbank
waitbut	lda #%00010000
	bit $dc00               ;Firebutton pressed?
        bne waitbut
        sei
        lda #$37
        sta $01			;reenable ROMs
        brk			;basic warmstart


checker_screen:	.(
	pha
	txa
	pha
	tsx
	lda $0102,x
	lda #102
	ldx #0
l28     sta $0400,x
        sta $0500,x
        sta $0600,x
        sta $0700,x
        inx
        bne l28
	pla
	tax
	pla
	rts
	.)
	
	;; Fill the colour ram with a value.
	;; I# A: colour code
	;; O#
	;; D#
fill_colour_ram:	.(
	pha
	txa
	pha
	tsx
	lda $0102,x
	ldx #0
l28     sta $d800,x
        sta $d900,x
        sta $da00,x
        sta $db00,x
        inx
        bne l28
	pla
	tax
	pla
	rts
	.)

init_sprite:
	jsr set_sprite_pointers
	jsr set_sprite_positions
	lda #%01100000		;Sprite 5,6 are multicolour
	sta $d01c
	lda #5
	ldx #7
l184	sta $d027,x		;Sprite colour is dark green
	dex
	bpl l184
	lda #$4
	sta $d027		;This sprite in purple.
	lda #$04
	sta $d025		;Sprite multicolour for 01
	lda #$07
	sta $d026		;Sprite multicolour for 11
	lda #$ff
	sta $d015		;turn all sprites on
	;; Double sprites in X. TODO: make configurable.
	lda #$ff
	sta	$d01d
	rts

	;; Set sprite pointers according to table
	;; I
	;; O X=$FF
	;; D A,X
set_sprite_pointers:	.(
	ldx #$07
l289:	lda sprite_pointers,x
	sta SPRITE_POINTERS,x
	dex
	bpl l289
	rts
	.)

	;; Set sprite positions accordings to table
	;; I
	;; O
	;; D A,X,Y
set_sprite_positions:	.(
	ldx #0
	ldy #0
	lda #0
	sta v189
l197:	lsr v189
	lda sprite_positions,x
l192:	ora v189
	sta v189
	inx
	lda sprite_positions,x
	sta $d000,y
	inx
	iny
	lda sprite_positions,x
	sta $d000,y
	inx
	iny
	cpy #16
	bcc l197
	lda v189
	sta $d010
	rts
v189:	.byte 0
	.)
	
interrupt_setup:
        sei        	;disable maskable IRQs
        lda #$7f
        sta $dc0d 	;disable timer interrupts which can be generated by the two CIA chips
        sta $dd0d	;the kernal uses such an interrupt to flash the cursor and scan the keyboard,
			;so we better stop it.
        lda $dc0d	;by reading this two registers we negate any pending CIA irqs.
        lda $dd0d	;if we don't do this, a pending CIA irq might occur after we finish setting up our irq.
			;we don't want that to happen.
        lda #$01   	;this is how to tell the VICII to generate a raster interrupt
        sta $d01a
        lda #<irq	;this is how we set up
        sta $314	;the address of our interrupt code
        lda #>irq
        sta $315
	LDA #<brknmi
	STA $FFFA
	LDA #>brknmi
	STA $FFFB
	rts

brknmi:	pha
	txa
	pha
	tya
	pha
	tsx
l148	lda #$FF
	eor $0100,x
	sta $0100,x
	dex
	bne l148
	tsx
	lda $0100
	eor #$ff
	sta $0100
	lda $0102,x
	clc
	sbc #2
	sta $0102,x
	bcc l156
	dec $0103,x
l156
	pla
	tay
	pla
	tax
	pla
	rti
	
irq:	dec $d021
 	lda #>irqtrick-1	;Push address minus one onto the stack so that the
				;RTS will return correctly
 	pha
 	lda #<irqtrick-1
 	pha
	inc $d021
	inc $d021
	jmp (interrupt_ptr)

irqtrick:	
	dec $d021
	jsr set_next_irq
	lda #$ff   ;this is the orthodox and safe way of clearing the interrupt condition of the VICII.
        sta $d019  ;if you don't do this the interrupt condition will be present all the time and you end
                ;up having the CPU running the interrupt code all the time, as when it exists the       
                ;interrupt, the interrupt request from the VICII will be there again regardless of the
                ;rasterline counter.
	jmp $EA81		;restore A/X/Y and end IRQ

	;; Set the next interrupt pointer
	;; I
	;; O
	;; D A,X
	;; Z interrupt_ptr
set_next_irq:	.(
 	ldx irqidx
	lda irqtable,x
	sta $d012
	inx
	lda irqtable,x
	sta interrupt_ptr
	inx
	lda irqtable,x
	sta interrupt_ptr+1
	inx
	cpx #irqtable_end-irqtable
	bcc no_clear
	ldx #0
no_clear:	stx irqidx
	rts
	.)

irq_disable_border:
	lda #$13		;24 lines
 	sta $d011
	rts
irq_reset_vic_border:
	lda #$1b		;25 lines
	sta $d011
	rts
irq_handle_joystick:
	inc $d020
	lda #$00		;Check every time
	ldx #$00		;sprite #0
	jsr getjoy
	beq l319
	dec end_of_program
l319	dec $d020
	rts
irq_print_position:
	inc $d021
	jsr SID_PLAY
	dec $d021
	rts
	
	;; Joystick testing routine
	;; I A=number of skipped tests, X=which sprite
	;; O A=1 if fire butten is pressed, zero flag is also set
	;; D A
getjoy:	.(
	dec joycounter		;decrement counter for skipped tests
	bmi dojoy		;If not less than zero then clear A and carry, e.g. no fire button
	lda #0
	clc
	rts
dojoy:	sta joycounter		;Reset the counter
	txa			;Multiply X with two
	asl
	tax
	lda $dc00		;Load joystick register
	lsr			;shift LSB, is up
	bcs no_up		;branch if joystick not pressed up
	dec $d001,x		;decrement sprite Y position, use X as index
no_up:	lsr			;shift LSB, is down
	bcs no_down
	inc $d001,x
no_down:	lsr		;shift LSB, is left
	bcs no_left
	dec $d000,x
	pha			;push processed joystick status
	lda $d000,x		;Load X position
	cmp #$ff		;underflow of position?
	bne l347
	lda v330,x		;load bit mask for sprite bit 9
	eor $d010		;Change the bit
	sta $d010		;and store it again
l347	pla
no_left:	lsr		;shift LSB, is right
	bcs no_right
	inc $d000,x
	bne no_right		;overflow of position?
	pha
	lda v330,x		;load bit mask for sprite bit 9
	eor $d010		;Change the bit
	sta $d010		;and store it again
	pla
no_right:			;now LSB has status of fire button
	and #$01
	eor #$01		;line goes low, therefore invert
	rts
joycounter:
	.byt 0
v330:	.byt $01,$01,$02,$02,$04,$04,$08,$08
	.byt $10,$10,$20,$20,$40,$40,$80,$80
	.)
	
;;; Set the current bank for the text display. The VIC is set accordingly.
;;; I A bits 0..3
;;; O value of $d018
txtbank	pha
	lda #$0F
	and $d018
	sta $d018
	pla
	asl
	asl
	asl
	asl
	ora $d018
	sta $d018
        rts
 
	
irqidx:	.byt	0		;Index into IRQ table
	;; Table consists of rasterline, pointer to routine at this line
irqtable:
	.byt $00
	.word irq_handle_joystick
	
	.byt $2a
	.word irq_reset_vic_border

	.byt $90
	.word irq_print_position
	
	.byt $f9
	.word irq_disable_border
irqtable_end:

;;; For the sprite positions have a look at figure 8-3 on page 66 in
;	the book by Sally Greenwood Larsen, "Sprite Graphics for the
;	Commodore 64", Micro Text Publications, 1983. In this book the
;	position of the border is described.
sprite_positions:
	.byt	%00000000, $50, $80
	.byt	%10000000, $00, $00
	.byt	%10000000, $40, $E5
	.byt	%10000000, $00+24*2, $37
	.byt	%10000000, $00+24*3, $fa
	.byt	%10000000, $00+24*1, $80
	.byt	%10000000, $00+24*2, $80
	.byt	%00000000, $18, $32

sprite_pointers:
	.byt spritechecker/$40	;select sprite block for spritechecker
	.byt spritechecker/$40
	.byt spritebox/$40	;select sprite block for a box
	.byt spritechecker/$40
	.byt spritechecker/$40
	.byt $0100/$40		;bottom of stack
	.byt $01C0/$40		;top of stack
	.byt spritebox/$40
	
        .dsb 16384-2*64-*, $00	
spritechecker:
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA,$55,$55,$55
	.byt	$AA,$AA,$AA
spritechecker_end:

        .dsb 16384-64-*, $00	
spritebox:
	.byt	$FF,$FF,$FF
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$80,$00,$01
	.byt	$FF,$FF,$FF
spritebox_end:	
