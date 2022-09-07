;;; Music file which uses the LOWCODE segment. This segment must start at $1000 for the typical SIDs.
	
	.export	_muzak
	.export	_muzak_init
	.export	_muzak_init00
	.export	_muzak_play

	.segment	"LOWCODE"
_muzak:	.incbin "../sid/bananas-01.sid",$7c+2

	_muzak_init = _muzak
	_muzak_play = _muzak+3

	.code
.proc	_muzak_init00
	lda	#$00
	jmp	_muzak_init
.endproc
