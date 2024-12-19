PAYLOAD_DESTINATION=$0801
PAYLOAD_JUMP=$080d
srcptr=$5C			; Source pointer in ZP
destptr=$5d			; Destination pointer in ZP
STUBSTART=$326
	*=STUBSTART

	;; https://codebase64.org/doku.php?id=base:autostarting_disk_files
	.word	boot
	.word $f6ed	;$328 kernal stop routine Vector ($f6ed)
	.word $f13e	;$32a kernal getin routine ($f13e)
	.word $f32f	;$32c kernal clall routine vector ($f32f)
	.word $fe66	;$32e user-defined vector ($fe66)
	.word $f4a5	;$330 kernal load routine ($f4a5)
	.word $f5ed	;$332 kernal save routine ($f5ed)

	;; Copy routine: L.A. Leventhal, W. Saville, "6502 Assembly Language Subroutines", Osborne/McGraw-Hill, 1982, p. 201.
MVSRCE:	.word	payload_begin
MVDEST:	.word	PAYLOAD_DESTINATION
MVELEN:	.word	payload_end-payload_begin
JUMP:	.word	PAYLOAD_JUMP
boot:
	;; Initialise pointers.
	lda	MVSRCE
	sta	srcptr
	lda	MVDEST
	sta	destptr
	lda	MVSRCE+1
	pha
	clc
	adc	MVSRCE+1
	sta	srcptr+1
	pla
	clc
	adc	MVDEST+1
	sta	destptr+1
	;; Move partial page first.
	ldy	MVELEN
	beq	only_full_pages
mr0:	dey
	lda	(srcptr),y
	sta	(destptr),y
	cpy	#0
	bne	mr0
only_full_pages:
	ldx	MVELEN+1
	beq	mrexit
mr1:	dec	srcptr+1
	dec	destptr+1
mr2:	dey
	lda	(srcptr)+1
	sta	(destptr)+1
	cpy	#0
	bne	mr2
	sta	$d020
	dex
	bne	mr1
mrexit:	jmp	(JUMP)

payload_begin:
	.fill	$80, $60
payload_end:

stubMVSRCE_offset = MVSRCE-STUBSTART
stubMVDEST_offset = MVDEST-STUBSTART
stubMVELEN_offset = MVELEN-STUBSTART
stubJUMPDEST_offset = JUMP-STUBSTART
