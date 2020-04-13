;;; cl65 logo.s

SRCPTR = $fe
DSTPTR = $fc
OFFSET = $fa
TMPSRC = $a3
TMPDST = $a5

	.segment	"INIT"
	.segment	"ONCE"
	.segment	"STARTUP"
main:
	lda #<logo
	sta SRCPTR
	lda #>logo
	sta SRCPTR+1
	lda #<$0400
	sta DSTPTR
	lda #>$0400
	sta DSTPTR+1
	lda #0
	sta OFFSET
	sta OFFSET+1
	lda #6
	sta $d020
@loop:
	jsr incbyt
	lda #17
	jsr addtooffset
	jmp @loop
	rts

addtooffset:
	clc
	adc OFFSET
	sta OFFSET
	bcc @skip
	inc OFFSET+1
@skip:
	lda OFFSET+1
	cmp #$04
	bcc @skip2
	lda #0
	sta OFFSET+1
@skip2:
	rts

incbyt:
	lda SRCPTR
	clc
	adc OFFSET
	sta TMPSRC
	lda SRCPTR+1
	adc OFFSET+1
	sta TMPSRC+1
	lda DSTPTR
	clc
	adc OFFSET
	sta TMPDST
	lda DSTPTR+1
	adc OFFSET+1
	sta TMPDST+1
	ldy #0
	lda (TMPDST),y
	cmp (TMPSRC),y
	beq @skip
	sec
	sbc #1
	sta (TMPDST),y
@skip:	rts


logo:	.incbin	"logo.t7d.screencodes"
