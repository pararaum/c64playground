	.include	"t7d/stringfunctions.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/stackmacros.i"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"
	.include	"../globals.i"

	.segment	"ONCE"
	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	cld
	sei
	jsr	_disable_cia_irq
	memoryconfig_charrom
	ldax	#$d000
	stax	ptr1
	ldax	#$d000
	stax	ptr2
	ldax	#$1000
	jsr	memcpy_up
	memoryconfig_kernal
	SwitchVICBank	3
	SetScreenMemory	MEMMAP_gfxarea
	ldx	#$ff
	txs
	ldax	#MEMMAP_stackpointer	; Set the C stack pointer, no further initialisation!
	stax	sp
	lda	#0
	jsr	$1000		; Init music.
	lda	#$FF
	sta	Control_single_mode
	lda	#>MEMMAP_gfxarea ; Set the text screen to $C000.
	sta	$288
	jsr	MEMMAP_democode
	delay_ms	6000
	jmp	64738
