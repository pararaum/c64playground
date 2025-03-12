	.include	"t7d/memoryfunctions.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/petsciicopyframe.i"
	.include	"t7d/memoryconfig.i"
	.include	"LAMAlib.inc"
	.include	"globals.i"
	.macpack	cbm

	.import	init_the_jump,init_high_powered_hanging,init_fly_free

	.export	PETSCIICOPY_SCREEN=$400
	.export	ELECTRICITY_INIT=$c048, ELECTRICITY_PLAY=$c021
	.export	SFXBASE=$1000, SFXPLAY=$1003

	.segment	"STARTUP"
	jsr	run_once
	sei
	memoryconfig_kernal
	CopyFileIntoMemoryDown	$c021,"Electricity.sid",$7c+2
	CopyFileIntoMemoryDown	ROTATED_FONT,"assets/cbmcharset-rotated.prg",2
	CopyFileIntoMemoryDown	SFXBASE,"sfx.1000",2
	jmp	main

	.segment	"INIT"
	.segment	"ONCE"
.proc	run_once
	lda	#$93		; Clear screen.
	jsr	CHROUT
	SmallMemCpy	text,textsize,__SCREEN_START__+40-textsize
	SmallMemSet	6,textsize,$D800+40-textsize
	rts
text:	scrcode	"special bzzzt to freeze"
textsize = *-text
.endproc


	.data
do_electricity_init:
	.byte	$ff

	.code
.proc	irq
	bit	do_electricity_init
	bvc	noinit
	lda	#0
	sta	do_electricity_init
	jsr	ELECTRICITY_INIT
noinit:	jsr	ELECTRICITY_PLAY
	lda	$d019
	and	#$7f
	sta	$d019
	jmp	$ea81
.endproc

	.code
main:	
	lda	#0
	jsr	SFXBASE
	SetIRQ314Pointer	irq
	jsr	init_high_powered_hanging
	jsr	init_fly_free
	jsr	init_the_jump
	jsr	_disable_cia_irq
	EnableIRQatRasterline	250
	cli
	jmp	*
