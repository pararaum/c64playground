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
	.include	"resident.i"
	.include	"t7d/sprite/spritepad.i"
	.include	"t7d/pseudo/yielding.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/theatre/consts.i"

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
	Memcpy1KInline	$400,MEMMAP_gfxarea
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
	jsr	Start
aurevoir:
	lda	finiflag
	beq	aurevoir
	lda	$d011
	and	#%01100000
	sta	$d011
	delay_ms	200
	lda	#$6
	sta	$d020
	delay_ms	200
	lda	#0
	sta	$d020
aloop:	delay_ms	100
	dec	$10e5
	bne	aloop
	jmp	*

	.import	_frame0000

	.export	PETSCIICOPY_SCREEN=MEMMAP_gfxarea

SPRY = 208
DELTAPEE = 17
PALMPEEX = 40

	.rodata
sidsprites:	.incbin	"../assets/sid-mascot.spd"
nextpart:	.asciiz "resurrection.Z"

	.data
sprx:	.byte	88
sprhx:	.byte	$FF		; MSB of sprites.
counter:	.byte	0
finiflag:	.byte	0
	.data
wctr:	.word	130		; Wait counter for Sid waiting before peeing.


	.code
.proc	update_spr
	lda	sprx
	sta	$d000
	sta	$d002
	lda	sprhx
	sta	$d010
	lda	counter
	lsr
	lsr
	lsr
	lsr
	and	#3
	switch	A
	case	0
	copySPadData	sidsprites,2,THEATRE_SPRT+64
	copySPadData	sidsprites,3,THEATRE_SPRT
	break
	case	1
	copySPadData	sidsprites,4,THEATRE_SPRT+64
	copySPadData	sidsprites,5,THEATRE_SPRT
	break
	case	2
	copySPadData	sidsprites,6,THEATRE_SPRT+64
	copySPadData	sidsprites,7,THEATRE_SPRT
	break
	case	3
	copySPadData	sidsprites,8,THEATRE_SPRT+64
	copySPadData	sidsprites,9,THEATRE_SPRT
	break
	endswitch
	rts
.endproc

.proc	init
	jsr	update_spr
	lda	#3
	sta	$d015		; Enable sprite zero and one.
	poke	$d029,3		; Teal pee, yeah, I know...
	;lda	#$ff
	;sta	$d01d		; Double width.
	;sta	$d017		; Double height.
	lda	#SPRY
	sta	$d001
	sta	$d003
	sta	$d005
	;;(/ #x800 64)32
	for Y,32,to,34
	 tya
	 sta	THEATRE_SPRT_PTR-32,y
	next
	rts
.endproc

.proc	irq_go_left_final
	dec	sprx
	bne	cont
	lda	$d011
	and	#%01101111	; Turn screen off.
	sta	$d011
	lda	#$ff
	sta	finiflag
cont:
	jsr	update_spr
	inc	counter
	rts
.endproc

.proc	irq_pee
	inc	counter
	bne	noendpee
	lda	#2
	jsr	$1000
	ldax	#irq_go_left_final
	jsr	RESIDENT_set_isr_call
	lda	#3		; Only two sprites, again.
	sta	PEENUMSPR
noendpee:
	lda	counter
	lsr
	lsr
	lsr
	and	#3
	switch	A
	case 0
	copySPadData	sidsprites,12,THEATRE_SPRT+128
	break
	case 1
	copySPadData	sidsprites,13,THEATRE_SPRT+128
	break
	case 2
	copySPadData	sidsprites,14,THEATRE_SPRT+128
	break
	case 3
	copySPadData	sidsprites,15,THEATRE_SPRT+128
	break
	endswitch
	;; Lifted leg
	copySPadData	sidsprites,10,THEATRE_SPRT+64
	copySPadData	sidsprites,11,THEATRE_SPRT
	lda	sprx
	sec
	adc	#DELTAPEE
	sta	$d004		; Sprite 3 position
	lda	#%00000111
	PEENUMSPR=*-1
	sta	$d015		; Enable three sprites.
	rts
.endproc

.proc	irq_wait
	copySPadData	sidsprites,0,THEATRE_SPRT+64
	copySPadData	sidsprites,1,THEATRE_SPRT
	dec16	wctr
	lda	wctr
	ora	wctr+1
	bne	endwait
	;;  A=0
	sta	counter
	ldax	#irq_pee
	jsr	RESIDENT_set_isr_call
	lda	#1
	jsr	$1000
endwait:
	rts
.endproc

.proc	irq_go_left2
	dec	sprx
	lda	sprx
	cmp	#PALMPEEX
	bne	cont
	ldax	#irq_wait
	jsr	RESIDENT_set_isr_call
cont:
	jsr	update_spr
	inc	counter
	rts
.endproc

.proc	irq_go_left
	dec	sprx
	bpl	nounderflow
	lda	#0
	sta	sprhx
	ldax	#irq_go_left2
	jsr	RESIDENT_set_isr_call
nounderflow:
	jsr	update_spr
	inc	counter
	rts
.endproc

	.code
.proc	copy_palm
	SetupYielding	yfunc,,25-1
yloop:
	delay_ms	300
	CallYieldingCheckEnd
	bne	yloop
	ldax	#irq_go_left
	jsr	RESIDENT_set_isr_call
	rts
yfunc:	ldax	#_frame0000+2+24*40
	stax	ptr1
	ldax	#PETSCIICOPY_SCREEN+24*40
	stax	ptr2
	ldax	#_frame0000+2+24*40+1000
	stax	ptr3
	ldax	#$d800+24*40
	stax	ptr4
	ldx	#25-1
l2:	ldy	#39
l1:	lda	(ptr1),y
	cmp	#$20
	beq	s1
	sta	(ptr2),y
	lda	(ptr3),y
	sta	(ptr4),y
s1:	dey
	bpl	l1
	P_sub	#40,ptr1
	P_sub	#40,ptr2
	P_sub	#40,ptr3
	P_sub	#40,ptr4
	Yield
	dex
	bpl	l2
	rts
.endproc


	.code
Start:	jsr	_disable_cia_irq
	CopyFileIntoMemoryDown $800, "resident", 2
	CopyFileIntoMemoryDown $1000, "assets/muletest.sid", $7c+2
	lda	#0
	jsr	$1000
	jsr	RESIDENT_set_default_irq
	jsr	init
	cli
	jsr	copy_palm
	lda	#0
	sta	$d027		; Sprite 0 colour.
	lda	#1
	sta	$d028		; Sprite 1 colour.
	rts
