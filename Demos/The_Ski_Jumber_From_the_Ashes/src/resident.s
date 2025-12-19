	.include	"resident.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/libt7d.i"
	.include	"LAMAlib.inc"
	.include	"t7d/compression/zx02.i"
	.include	"dload-labels.inc"
	.include	"globals.i"

	;; Our own pointers and temporaries.
	.segment	"ZEROPAGE"
ptr1:	.res	2
ptr2:	.res	2
ptr3:	.res	2
ptr4:	.res	2
tmp1:	.res	1
tmp2:	.res	1
tmp3:	.res	1
tmp4:	.res	1

	;; Export the zeropage addresses
	.exportzp	ptr1
	.exportzp	ptr2
	.exportzp	ptr3
	.exportzp	ptr4
	.exportzp	tmp1
	.exportzp	tmp2
	.exportzp	tmp3
	.exportzp	tmp4

	.bss
last_options:	.res	1

	.code
.proc	irq
irq:	pha
	txa
	pha
	tya
	pha
from314:
	lda	1		; Old memory config
	sta	MEMCNFPTR
	memoryconfig_io
	asl	$d019
	jsr	myrts
	JSRPTR=*-2
	jsr	$1003
	lda	#$37		; Self-modifying code!
	MEMCNFPTR=*-1
	sta	1		; Restore memory configuration.
	pla
	tay
	pla
	tax
	pla
myrti:	rti
myrts:	rts
.endproc

.proc	setup_default_irq
	ldax	#myrts
	stax	irq::JSRPTR
	ldax	#irq::myrti
	stax	$318
	php
	sei
	SetIRQCPUPointer	irq
	SetIRQ314Pointer	irq::from314
	plp
	EnableIRQatRasterline	250
myrts:	rts
.endproc

.proc	setup_isr_call
	php
	sei
	stax	irq::JSRPTR
	plp
	rts
.endproc

	;; Setup and include xipz decompressor.
        .define XIPZSRCPTR ptr1
        .define XIPZDSTPTR ptr2
        .define XIPZAUXPTR ptr3
	.include	"t7d/compression/xipz-qadz.decrunch.inc"

.proc	decrunch_zx02
	stax	ptr1		; Source
	PullStoreStackptrLOCAL	; Get return address from stack and store it.
	pla
	sta	ptr2+1
	pla
	sta	ptr2
	jsr	zx02decrunch
	RetrievePushStackptrLOCAL
	rts
.endproc

	;; TODO: Check why BIT behaves not as expected... Use AND?
.proc	load_nextpart
	stax	fnamptr
	sty	last_options
	bit	Control_single_mode
	bvc	nosingle
	bit	last_options
	bpl	*
	rts
nosingle:
	memoryconfig_io		; Changes only A.
	lda	#RESIDENT_keep_irq
	and	last_options
	bne	no_default_irq
	jsr	setup_default_irq
no_default_irq:
	lda	last_options	; Move last_options to A.
	lsr			; Move lowest bit of last_options to carry
	ldax	#MEMMAP_hiload
	bcc	noloadlow
	ldax	#MEMMAP_loload
noloadlow:
	stax	LdLAE
	lda	#$40
	sta	LdZp_PresetAE
	ldax	fnamptr
	strlen8	AX
	txa
	Load16	x,y,fnamptr
	jsr	LdLoc
	bcc	okloader
	lda	#2
	sta	$d020
	jmp	*
okloader:
	lda	#RESIDENT_load8000	; Check again where we did load the data.
	bit	last_options
	bpl	noreturn
	rts
noreturn:
	jmp	decrunch_last_loaded
	.pushseg
	.bss
fnamptr:	.res	2
	.popseg
.endproc

.proc	decrunch_last_loaded
	bit	Control_single_mode
	bvc	nostandalone
	;; Endless flicker when in standalone mode, as no next part to uncrunch.
endless:
	inc	$d020
	dec	$d020
	dec	$d020
	inc	$d020
	bvs	endless
nostandalone:
	lda	#RESIDENT_load8000	; Check again where we did load the data.
	and	last_options
	if eq			; "eq" means zero flag is set, therefore we load at $E000.
	 ldax	#MEMMAP_hiload+2
	else
	 ldax	#MEMMAP_loload+2
	endif
	stax	ptr1		; Store decrunch pointer data in ptr1 for safekeeping.
	lda	last_options	; Get last_options.
	lsr			; Move one bit to the right.
	and	#3		; Mask out bits for decrunch.
	switch	A
	case 0
	NOP
	break
	case 1
	ldax	ptr1		; Get data pointer.
	jsr	pudecrunch_default
	break
	case 2
	ldax	#MEMMAP_democode
	stax	ptr2
	jsr	zx02decrunch
	break
	case 3
	ldax	#MEMMAP_democode
	stax	ptr2
	jsr	decrunch_qadz
	break
	endswitch
	jmp	MEMMAP_democode
.endproc

.proc	copy_datasette
	sty	tmp1	; Store last_options.
	stax	ptr1	; Source pointer.
	ldax	#$33c
	stax	ptr2	; Destination pointer.
	jsr	memcpy1K_via_ptr
	lda	#RESIDENT_datasette_default_irq
	and 	tmp1	; Test if the bit is set.
	beq	s1		; Bit is not set.
	jsr	setup_default_irq
s1:	bit 	tmp1	; Check again the option.
	bvc	s2		; Bit 6 not set.
	jsr	setup_default_irq
	ldax	#$33f
	jsr	setup_isr_call
s2:	bit 	tmp1	; Check last_options, again.
	bpl	s3		; Bit 7 set, so no JMP to cassette buffer.
	jmp	$33c
s3:	rts
.endproc

	.segment	"EXEHDR"
	;; 0
	jmp	setup_default_irq
	;; 3
	jmp	setup_isr_call
	;; 6
	jmp	decrunch_qadz
	;; 9
	jmp	decrunch_zx02
	;; 12
	jmp	load_nextpart
	;; 15
	jmp	decrunch_last_loaded
	;; 18
	jmp	pudecrunch_default
	;; 21
	jmp	copy_datasette
