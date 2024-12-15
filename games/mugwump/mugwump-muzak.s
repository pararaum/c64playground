	.export	muzak_init, muzak_play

	.segment	"MUZAK"
muzak_init:	.incbin	"Mugwump.sid",2+$7c
muzak_play = muzak_init + 3
