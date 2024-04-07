	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/stringfunctions.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"LAMAlib-macros16.inc"
	;; 	.include	"loadersymbols-c64.inc"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"dload-labels.inc"
	.include	"globals.i"

	LOADERINIT := LdCfgInit_CodeStart

	.importzp	ptr1,ptr2,ptr3

	.segment	"INIT"
	.segment	"ONCE"
	.segment	"STARTUP"
	jmp	entry
	.res	$1000

	.segment	"LOWCODE"
copy:
	jsr	copy_memory_downwards_ip
	.word	IRQORG
	.word	@irqend-@irqbegin
@irqbegin:
	asl	$d019
	jsr	$1003
	jmp	$ea31
@irqend:
	.assert	*>$1000,error
	CopyFileIntoMemoryDown	RESIDENT, "resident.prg", 2
	.assert	*>=LOADERINIT,error
	CopyFileIntoMemoryDown	LOADERINIT, "dload.prg", 2
	rts

	.code
entry:	jsr	copy
	jsr	LOADERINIT
	bcc	ok
	ldax	#errortext
	jmp	output_long_string
errortext:
	.asciiz	"can not install loader!"
ok:	CopyFileIntoMemoryDown	$1000, "HAMURABI.sid", 2+$7c
	memoryconfig_kernal
	jsr	_disable_cia_irq
	lda	#0
	jsr	$1000
	sta	$d011		; Disable screen.
	sei
	SetIRQ314Pointer	IRQORG
	EnableIRQatRasterline	0
	cli
	jmp	RESIDENT
