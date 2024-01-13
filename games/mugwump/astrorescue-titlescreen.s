	.export	screen_color_data

	.segment	"SCREEN"
	.incbin	"astrorescue-titlescreen.bin",0,1000

	.segment	"ONCE"
screen_color_data:
	.incbin	"astrorescue-titlescreen.bin",1000,1000
