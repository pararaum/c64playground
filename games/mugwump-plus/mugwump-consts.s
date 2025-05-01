
	.export	__FONT_START__ = $800
	.export	SCREEN_AND_BORDER_COL
	.export credits_screen
	.export	title_font
	.export	credits_font
	.export	game_font

	.rodata
SCREEN_AND_BORDER_COL:	.byte 0

game_font:
title_font:
	.incbin	"Prince.upper.64c", 2

credits_font:
	.incbin	"Prince.both.64c", 2

credits_screen:
	.incbin	"credits-petscii.tmp", 4, 1000
