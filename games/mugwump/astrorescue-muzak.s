	.export	muzak_init, muzak_play

	.segment	"MUZAK"
muzak_init:	.incbin	"space_glider_PAL.sid",2+$7c
muzak_play = muzak_init + 3

