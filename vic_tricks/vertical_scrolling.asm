	.word $0801
	* = $0801
	
basic:	.(
	.word end_of_basic
	.word main
	.byte $9e,$20,$c2
	.asc "(57)",$aa,"256",$ac,$c2,"(58)"
end_of_basic:
	.byte 0,0,0
	.)


fill_screen:	.(
	lda #$66
	ldy #0
loop:	sta $0400,y
	sta $0500,y
	sta $0600,y
	sta $0700,y
	iny
	bne loop
	lda #<$0400
	sta $fe
	lda #>$0400
	sta $ff
	lda #$00
	ldx #$ff
l2:	inx
	txa
	ldy #$00
	sta ($fe),y
	iny
	sta ($fe),y
	iny
	sta ($fe),y
	lda $fe
	clc
	adc #40
	bcc no_ovl
	inc $ff
no_ovl:	sta $fe
	lda $ff
	cmp #$08
	bne l2
	rts
	.)

strange:	.(
	lda #$18
	sta $d011
l1:	lda $d012
	sta $d020
comp:	cmp #$31
	bne l1
	lda #$1f
	sta $d011
	inc $d020
	dec $02
	bpl skipjoy
	lda #$4
	sta $02
	lda	$DC00
	lsr
	bcs noup
	dec comp+1
noup:	lsr
	bcs nodown
	inc comp+1
nodown:	nop
skipjoy:	
	rts
	.)

main:	ldx #$02
	stx $d020
	sei
	jsr fill_screen
	inc $d020
wattos:	bit $d011
	bmi wattos
	lda #$07
	sta $d020
	lda #$1b
	sta $d011
	jsr strange
	lda #249
wait_border:	cmp $d012
	bne wait_border
	inc $d020
	lda #$13
	sta $d011
still_upper:	bit $d011
	bpl still_upper
	inc $d020
	jmp wattos
    	rts
