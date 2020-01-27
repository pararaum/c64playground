
	XPOS = $2
	SCREENY = $0400+40*4

	.word $0801
	*=$0801
	.word end_of_basic
	.word main
	.byte $9e,"(2064)"
end_of_basic:
	.byte 0,0,0
	jmp main

meteor:	.byte	$51,$46,$40,$44,$45,$77,$20
meteor_end:
	meteor_size = meteor_end - meteor

main:	jsr clear
lo2:	ldx #$27
	stx XPOS
lo1:	ldx XPOS
	jsr draw
	jsr wait
	dec XPOS
sm1:	lda #<(-meteor_size)
	cmp XPOS
	bne lo1
	lda #' '
	sta SCREENY
	jmp lo2
	rts

wait:	.(
l1:	bit $d011
	bmi l1
	inc $d020
l2:	bit $d011
	bpl l2
	dec $d020
	rts
	.)

draw:	.(
	ldy #0
l1:	lda meteor,y
	cpx #0
	bmi s1
	sta SCREENY,x
s1:	inx
	cpx #40
	bpl out
	iny
	cpy #meteor_size
	bne l1
out:	rts
	.)
	
clear:	.(
	lda #$66
	ldx #0
l1:	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	dex
	bne l1
	rts
	.)
