
	.export	lfsr16_seed
	.export	lfsr16_call
	.export	lfsr16_set_feedbackterm

	.data
lfsr16_register:
	.word	$F77D		; Seed to be usable right away.

	.data
;;; http://users.ece.cmu.edu/~koopman/lfsr/16.txt
lfsr16_feedbackterm:
	.word	$8117

lfsr16_set_feedbackterm:
	sta	lfsr16_feedbackterm
	stx	lfsr16_feedbackterm+1
	rts

lfsr16_seed:
	sta	lfsr16_register
	stx	lfsr16_register+1
	rts

lfsr16_call:
	lsr	lfsr16_register+1 ; Shift HI to the right.
	ror	lfsr16_register	  ; Shift rest (LO) to the right.
	bcc	@not		  ; No Term to XOR.
	lda	lfsr16_feedbackterm
	eor	lfsr16_register
	sta	lfsr16_register
	lda	lfsr16_feedbackterm+1
	eor	lfsr16_register+1
	sta	lfsr16_register+1
@not:
	ldx	lfsr16_register+1
	lda	lfsr16_register
	rts
