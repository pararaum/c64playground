	.include	"moiety.arc.i"
	.include	"globals.i"
	.include	"t7d/libt7d.i"
	.include	"LAMAlib.inc"
	.include	"t7d/prng.lfsr.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/sid/sidmacros.i"

	.import	IRQMUZAKCALL

	.data
bzzzt:	.byte	$81

	.code
.proc arc_init
	ldax	#Arc_screen0
	ldy	#$67
	jsr	fill_1000_bytes
	ldax	#Arc_screen1
	ldy	#$76
	jsr	fill_1000_bytes
	ldax	#$8679
	jsr	lfsr16_set_feedbackterm
	ldax	#$FFFE
	jsr	lfsr16_seed
	SetVoiceRawFreq	1,433
	SetVoiceRawFreq	2,430
	SetVoiceRawFreq	3,436
	SetVoiceControl	1,$80
	SetVoiceControl	2,$80
	SetVoiceControl	3,$80
	lda	#$2c		; BIT
	sta	IRQMUZAKCALL
	rts
.endproc
	
.proc arc_run
	do_skip_every	3
	lda	#$80^$40
	eor	bzzzt
	sta	bzzzt
	.repeat	3,I
	sta	$d404+I*7
	.endrepeat
	rts
	end_skip_every
	jsr	lfsr16_call
	and	#1
	beq	zeroA
	SetScreenMemory	Arc_screen1
	jmp	nonzeroA
zeroA:
	SetScreenMemory	Arc_screen0
nonzeroA:
	SetVoiceRawFreq	1,433
	SetVoiceRawFreq	2,420
	SetVoiceRawFreq	3,416
	rts
.endproc
