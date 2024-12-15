	.include	"zeropage.inc"
	.include	"LAMAlib-macros16.inc"
	.include	"LAMAlib-routines.inc"
	.include	"t7d/kernal.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/libt7d.i"

	.import	__MUZAK_RUN__
	.import	__FONT_RUN__
	.import	setup_basic_program
	.import	_mainloop
	.import	mainirq
	.exportzp	sp, tmp1, tmp2, tmp3, tmp4, ptr1, ptr2, ptr3, ptr4, sreg, regsave, regbank

BACKGROUNDCOLOR = 0
FOREGROUNDCOLOR = 1

	.segment	"ZEROPAGE"
tmp1:	.res 1
tmp2:	.res 1
tmp3:	.res 1
tmp4:	.res 1
ptr1:	.res 2
ptr2:	.res 2
ptr3:   .res    2
ptr4:   .res    2
sreg:   .res    2       ; Secondary register/high 16 bit for longs
regsave:        .res    4       ; Slot to save/restore (E)AX into
regbank:	.res	regbanksize

	.segment	"LOWZP"
sp:	.res	2		; Reserve for C-stack pointer.

	.segment	"EXEHDR"
	sei
	jsr	setup_irq
	jsr	setup_font
	jsr	setup_c_stack
	lda	#0
	jsr	__MUZAK_RUN__
	jsr	setup_basic_program
	lda	background
	sta	$d020
	sta	$d021
	lda	foreground
	sta	$286		; current color, https://sta.c64.org/cbm64mem.html
	ldx	#$5F		; Keep away from the stack used by BASIC.
	txs
	ldy	#0
	lda	#$88
loop:	sta	$100,y
	dey
	bne	loop
	cli
	lda	#0
	tax
	tay
	jmp	_mainloop

background:	.byte	BACKGROUNDCOLOR
foreground:	.byte	FOREGROUNDCOLOR


.proc	setup_irq
	jsr	_disable_cia_irq
	SetIRQ314Pointer	mainirq
	EnableIRQatRasterline	0
	rts
.endproc


.proc	setup_c_stack
	;; Setup very small C stack in the cassette buffer.
	ldax	#$400
	stax	sp
	rts
.endproc


.proc	setup_font
	SetChargenAddress	__FONT_RUN__
	rts
.endproc

	.segment	"FONT"
	.incbin	"assets/Prophecy.upper.64c",2
