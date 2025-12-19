	.include	"../dload-labels.inc"
	.include	"t7d/stringfunctions.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/vic/memconf.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/stackmacros.i"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"
	.include	"../globals.i"

LOADAREA=$5200

	.segment	"ONCE"
	.segment	"EXEHDR"
	cld
	sei
	;; Copy twice as after installation of the loader the screen
	;; is at $C400 but afterwards the theater needs it at $C000 which
	;; we also use for the takeover.
	ldax	#$400
	stax	ptr1
	ldax	#MEMMAP_gfxarea
	stax	ptr2
	ldax	#1000
	jsr	memcpy_up
	ldax	#$400
	stax	ptr1
	ldax	#MEMMAP_gfxarea+$400
	stax	ptr2
	ldax	#1000
	jsr	memcpy_up
	memoryconfig_charrom
	ldax	#$d000
	stax	ptr1
	ldax	#$d000
	stax	ptr2
	ldax	#$1000
	jsr	memcpy_up
	memoryconfig_kernal
	ldx	#$ff
	txs
	ldax	#MEMMAP_stackpointer	; Set the C stack pointer, no further initialisation!
	stax	sp
	jsr	_main
	delay_ms	6000
	jmp	64738

	.rodata
errortext:	.asciiz	"can not install loader!"
firstname:	.byte	"takeover.Z"
firstname_end:
sekritname:	.byte	"private.Z"
sekritname_end:
	.code
_main:	jsr	LdCfgInit_CodeStart
	bcc	okloader
	ldax	#errortext
	jsr	output_long_string
	lda	#2
	sta	$d020
	rts
okloader:
	lda	#>MEMMAP_gfxarea
	jsr	set_screen_memory
	ldax	#LOADAREA
	stax	LdLAE
	lda	#$40
	sta	LdZp_PresetAE
	lda	#$10
	cmp	$400+1000-1
	beq	sekrit
	lda	#firstname_end-firstname
	Load16	x,y,#firstname
	jsr	LdLoc
	bcc	okfirst
	jmp	64738
sekrit:	lda	#sekritname_end-sekritname
	Load16	x,y,#sekritname
	jsr	LdLoc
	bcc	okfirst
	jmp	64738
okfirst:
	disable_NMI
	PushWordHL	#MEMMAP_democode-1	; Push address onto stack so that decrunch returns there.
	ldax	#LOADAREA+2
	jmp	pudecrunch_default
