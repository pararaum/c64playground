;;; Display each rasterline in a different colour. In debug mode we can see the delay.
;;; xa rastlines.asm

	CHROUT = $FFD2
	CHAROUTVECTOR = $f1ca

	.word $0326
	* = $326

	.word main	;autostart from charout vector ($f1ca)
	.word $f6ed	;$328 kernal stop routine Vector ($f6ed)
	.word $f13e	;$32a kernal getin routine ($f13e)
	.word $f32f	;$32c kernal clall routine vector ($f32f)
	.word $fe66	;$32e user-defined vector ($fe66)
	.word $f4a5	;$330 kernal load routine ($f4a5)
	;; cassette buffer

main:	.(
	;; Now restore the CHROUT vector.
	lda #<CHAROUTVECTOR
	sta $326
	lda #>CHAROUTVECTOR
	sta $327
	jsr message
	sei
	ldx $d012
	clv
l1:	cpx $d012
	bne l1
	stx $d020
	inx
	bvc l1
	rts
	.)

message:	.(
	ldy #0
l1:	lda txtmessage,y
	beq out
	jsr CHROUT
	iny
	bne l1
out:	rts
	.)

txtmessage:	.asc 13,"NOW TURN ON THE VIC SCREEN-BORDER DEBUG-MODE AND RELOAD.",0
