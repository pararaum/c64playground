	.include	"t7d/libt7d.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/memoryfunctions.i"
	.include	"LAMAlib.inc"
	.include	"zeropage.inc"

RASTERLINE=48
GFXMEMORY=$e000
TXTMEMORY=$CC00

	.export	irq
	.export	slice_next_image

	.rodata
image0:	.incbin	"guitar-heroine.prg",$2,8000
image1:	.incbin	"skeleton-heroine.prg",$2,8000

	.data
cycskip:	.byte	0	; Cycle skip for dma effect.

	.segment	"ONCE"
	.segment	"INIT"
	.segment	"STARTUP"
	jsr	_disable_cia_irq
	sei
	lda	#3
	sta	$d020
	jmp	main

	.code

;;; JSR=6,RTS=6
.proc	wait12
	rts
.endproc

.proc	wait40
	jsr	wait12
	jsr	wait12
	bit	*
	rts
.endproc

.proc	irq
	;; RASTERLINE
	PushRegs		;10
	lda	$d012		;4
	cmp	#RASTERLINE	;2
	bne	irq2		;2
	inc	$d012		;6
	asl	$d019		;6
	bit	$2c2c		;4
	nop			;2
	tsx			;2
	cli			;2
	.res	13,$EA
irq2:				; Now at cycle 32.
	;; RASTERLINE+1
	txs			;2
	ldx	#3		;2
@l1:	dex			;2
        bne	@l1		;3
        ;; @50
	nop			;2
	nop			;2
	NOP			;2
        lda $d012		;4
        cmp $d012		;4
        beq	@sync		;2/3
@sync:
	;; RASTERLINE+2
	;; At cycle 3.
	;; Disable bad line at row 51.
	lda	#$11		;2
	sta	$d011		;4
	ldx	#$3b		;2
	jsr	wait12		;12
	nop			;2
	nop			;2
	nop			;2
	bit	$EA		;3
	inc	$d020		;6
	lda	#39		;2
	sec			;2
	sbc	cycskip		;4
	lsr			;2
	sta	CSKIPBVAL	;4
	clv			;2
	bcc	*+2		;Even/Odd
	bvc	*
	CSKIPBVAL=*-1
	.res	20,$EA		;20 NOPs
	;; Variable delay code ends here.
	dec	$d011		; See code at https://codebase.c64.org/doku.php?id=base:horizontal_screen_positioning_hsp
	inc	$d011
	stx	$d011		; Activate DMA.
	LDX	0		; Choose dark blue as colour.
	;;
	do_every	20
	ldx	cycskip
	inx
	cpx	#40
	if eq
	ldx	#0
	endif
	stx	cycskip
	end_every

	;;
	lda	#0
	sta	$d020
	lda	#RASTERLINE
	sta	$d012
	lda	#$3b
	sta	$d011
	asl	$d019
	inc	$d020
	jsr	slice_next_image
	lda	#3
	sta	$d020
	lda	#6
	sta	$d021
	PullRegs
	rti
.endproc

.proc	init
	SetIRQCPUPointer	irq
	EnableIRQatRasterline	::RASTERLINE
	memoryconfig_io
	jsr	_disable_cia_irq
	SwitchVICBank 3
	SetHiresBitmapMode
	SetBitmapAddress ::GFXMEMORY
	SetScreenMemory ::TXTMEMORY
	asl	$d019
	cli
	rts
.endproc

.proc	slice_next_image
	ldx	cycskip
	beq	zero
	dex
	txa			; Get cycle skip into A as this is the number of bytes we need to skip.
	eor	#$FF		; 40-A, synthetic instruction as in https://www.nesdev.org/wiki/Synthetic_instructions.
	sec
	adc	#40-1
	asl			;*8
	asl
	asl
	sta	ptr3		; LO into spare pointer.
	lda	#0
	adc	#0
	sta	ptr3+1		; HI into spare pointer. 320=256+64
	lda	ptr3
	sta	ptr2		; Bitmap LO.
	lda	#>GFXMEMORY
	clc
	adc	ptr3+1
	sta	ptr2+1		; Bitmap HI.
	lda	ptr3
	clc
	adc	#<image1	; Source LO.
	sta	ptr1
	lda	ptr3+1
	adc	#>image1
	sta	ptr1+1		; Source HI.
	ldx	#200/8		; 200 rows to clear, we clear seven rows in the inner loop.
	lda	#0
l2:	ldy	#7
l1:	lda	(ptr1),y
	sta	(ptr2),y
	dey
	bpl	l1
	inc16	ptr1,320	; Go to next block.
	inc16	ptr2,320	; Go to next block.
	dex
	bne	l2
zero:
	rts
.endproc

.proc copy_image
	ldax	#image0
	stax	ptr1
	ldax	#GFXMEMORY
	stax	ptr2
	ldax	#8000
	jsr	memcpy_up
	ldy	#$76
	ldax	#TXTMEMORY
	jsr	fill_1000_bytes
	rts
.endproc

main:
	jsr	copy_image
	jsr	init
	ldx	#0
	ldy	#0
mainloop:
	nop			;2
	bit	$EA		;3
	lda	$400		;4
	inc	$400,x		;7
	rol	$401
	jmp	mainloop
