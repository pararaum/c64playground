; -*- mode: asm -*-

.macro	SetVoiceRawFreq	voice, freq
	.if voice<1 .or voice>3
	.fatal "Illegal voice!"
	.else
	lda	#<(freq)
	sta	$d400+voice*7
	lda	#>(freq)
	sta	$d401+voice*7
	.endif
.endmacro

.macro	SetVoiceControl	voice, control
	.if voice<1 .or voice>3
	.fatal "Illegal voice!"
	.else
	lda	#(control)
	sta	$d404+voice*7
	.endif
.endmacro
