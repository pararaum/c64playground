	.include	"t7d/memoryfunctions.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/theatre/consts.i"
	.include	"LAMAlib-routines.inc"

	.export _muzak_init = $1000
	.export _muzak_play = $1003
	.export	_font_data  = $0800

	.segment	"STARTUP"

	;; Will be positioned at $080d... and called via SYS 2061.

	disable_NMI
	lda	#0
	sta	$d011
	lda	#1
	sta	$d020
	sei
	memoryconfig_ram
	lda	#<(THEATRE_TEXT) ; Setup the current screen high byte.
	sta	$288
	CopyFileIntoMemoryDown	_font_data,"Orbiter Bold.both.64c",2
	CopyFileIntoMemoryDown	_muzak_init,"La_Fusiona_de_Dangera.sid",2+$7c
	memoryconfig_kernal
	cli
	;; Fall through to the actual startup code!!!
	NOP
