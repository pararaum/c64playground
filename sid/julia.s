;;; Julia by fieserWolf
	.export	SIDSONG_BEGIN
	.export	SIDSONG_END
	.export	PLAYERBACKGROUND
	.export	PLAYERFOREGROUND
	.export	PLAYERBORDER
	.export	SONGNAME
	.export	SONGAUTHOR
	.export	SONGRELEASED
	.export SONGLOADADDRPTR

	.rodata
PLAYERBACKGROUND:	.byte	1
PLAYERFOREGROUND:	.byte	6
PLAYERBORDER:		.byte	1
SONGNAME:	.word	SIDSONG_BEGIN+$16
SONGAUTHOR:	.word	SIDSONG_BEGIN+$36
SONGRELEASED:	.word	SIDSONG_BEGIN+$56
SONGLOADADDRPTR = SIDSONG_BEGIN+$7c

	.segment	"FONT"
	;; 	.incbin	"Scribe Bold.bin",0,$400
	.incbin	"../fonts/damieng/Polaris.bin",0,$400

	.segment	"MUZAK"
SIDSONG_BEGIN:
	.incbin	"julia.sid"
SIDSONG_END:
