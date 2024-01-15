	.include	"t7d/basic.i"
	.include	"t7d/kernal.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/vic/vicmacros.i"

	.importzp	ptr1, ptr2, ptr3, tmp1

	.segment	"STARTUP"
	sei
	jmp	entry

	.segment	"INIT"
	.segment	"ONCE"

	MUZAKADDR = $1000
	MUZAKINIT = $1000
	MUZAKPLAY = $1003

	.segment	"LOWCODE"
copy_data:
	CopyFileIntoMemoryDown $0400, "memory_copy_routine.test_screen.pet", 5, 1000
	CopyFileIntoMemoryDown $0800, "../fonts/damieng/Firebird.bin"
	CopyFileIntoMemoryDown MUZAKADDR, "../sid/bananas-01.sid",$7c+2
	CopyFileIntoMemoryDown $d800, "memory_copy_routine.test_screen.pet", 1005, 1000
	rts

	.code
irq:	asl	$d019
	bit	$d020
	bit	$d020
	bit	$d020
	dec	$d020
	jsr	MUZAKPLAY
	inc	$d020
	jmp	$ea31

entry:	jsr	copy_data
	lda	#0
	jsr	MUZAKPLAY
	lda	#1
	sta	$d020
	sta	$d021
	SetIRQ314Pointer irq
	jsr	_disable_cia_irq
	EnableIRQatRasterline 50
	cli
	SetChargenAddress	$800
	jmp	*
