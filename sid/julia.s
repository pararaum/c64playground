
	.export	SIDSONG_BEGIN
	.export	SIDSONG_END
	
	.segment	"FONT"
	;; 	.incbin	"Scribe Bold.bin",0,$400
	.incbin	"../fonts/damieng/Polaris.bin",0,$400

	.segment	"MUZAK"
SIDSONG_BEGIN:
	.incbin	"julia.sid"
SIDSONG_END:
