
	.export	SIDSONG_BEGIN
	.export	SIDSONG_END
	
	.segment	"FONT"
	.incbin	"../fonts/damieng/Polaris.bin",0,$400

	.segment	"MUZAK"
SIDSONG_BEGIN:
	.incbin	"bananas-01.sid"
SIDSONG_END:
