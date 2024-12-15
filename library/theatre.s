	.include	"t7d/vic/vicmacros.i"
	.include	"t7d/copy_chargen.i"
	.include	"t7d/libt7d.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/stackmacros.i"
	.include	"t7d/kernal.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/sprite/sprite.i"
	.include	"t7d/memoryconfig.i"
	.include	"t7d/pseudo/pointer.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.include	"t7d/theatre.i"
	.include	"LAMAlib-macros16.inc"
	.include	"LAMAlib-structured.inc"

	.importzp	ptr1, ptr2, ptr4
	.importzp	tmp1, tmp2, tmp3
	.import	popa, popax

	.export	CHARGEN_DESTINATION = THEATRE_FONT
	.export	_theatre_init
	.export	_theatre_wait_frame
	.export	_theatre_sprite_pos64
	.export	_theatre_switch_to_text
	.export _act_init
	.export	_scene_init
	.export	_theatre_frame_counter
	.export	_theatre_irq_isr_vector
	.exportzp	_theatre_universal_pointer

;;; This is the rasterline the interrupt is fired. It should be chosen in a way that there is enough time for all the deeds to be done.
RASTERLINE_FOR_INTERRUPT = 250

	.bss
;;; framecounter
_theatre_frame_counter:	.res	2
muzak_play_ptr:	.res	2

;;; Pointer to the animation data
animation_ptr:	.res	2

	.zeropage
_theatre_universal_pointer:	.res	2

	.data
;;; When the theatre interrupt-routine has finished this pointer is checked and if not zero the routine is called. It should return with a RTS. Warning! Only the zeropage registers tmp1, tmp2, ptr1, ptr2 are saved. If any other zeropage address is used then chaos will ensue.
	;; If zero no call is made.
_theatre_irq_isr_vector:	.word	0

	.code
.proc	_theatre_switch_to_text
	SwitchScreenAndChargenAddress	::THEATRE_TEXT, ::THEATRE_FONT
	lda	$d011		; Turn off bitmap mode
	and	#%01011111
	sta	$d011
	lda	$d016		; Turn off multicol mode
	and	#%11101111
	sta	$d016
	rts
.endproc

.proc _theatre_sprite_pos64
	asl
	sta	tmp2		; Store sprite number*2
	jsr	popax		; Y position
	stx	tmp1		; HI
	asl
	rol	tmp1
	asl
	rol	tmp1
	ldx	tmp2		; Sprite number*2
	lda	tmp1
	sta	$d001,x
	jsr	popax		; X position
	stx	tmp1
	asl
	rol	tmp1
	asl
	rol	tmp1
	;; C is bit 8 of sprite position.
	ldx	tmp2		; Sprite number*2
	lda	tmp1
	sta	$d000,x		; X-pos bits 0-7
	;; C is still as above!
	bcc	no_msb8
	lda	tmp2		; Sprite number*2
	lsr
	tax
	lda	SPRITE_HIGH_BITS,x
	ora	$d010
	sta	$d010
	rts
no_msb8:
	lda	tmp2		; Sprite number*2
	lsr
	tax
	lda	SPRITE_HIGH_BITS,x
	eor	#$ff
	and	$d010
	sta	$d010
	rts
.endproc

	.code
irq:
	lda	1
	pha
	memoryconfig_io
	.ifdef	DEBUG
	lda	$d020
	pha
	inc	$d020
	.endif
	asl	$d019
	P_inc	_theatre_frame_counter
	lda	ptr1
	pha
	lda	ptr1+1
	pha
	lda	ptr2
	pha
	lda	ptr2+1
	pha
	lda	tmp1
	pha
	lda	tmp2
	pha
	P_transfer	animation_ptr, ptr1
	jsr	theatre_animate_8
	PointerTestAndCall _theatre_irq_isr_vector
	pla
	sta	tmp2
	pla
	sta	tmp1
	pla
	sta	ptr2+1
	pla
	sta	ptr2
	pla
	sta	ptr1+1
	pla
	sta	ptr1
	.ifdef	DEBUG
	inc	$d020
	.endif
	PointerTestAndCall	muzak_play_ptr
	.ifdef	DEBUG
	pla
	sta	$d020
	.endif
	pla
	sta	1
	rts
irq314:
	jsr	irq
	jmp	$EA31

;;; This routine will save our ass when switching to RAM only mode.
.proc	irqCPU
	.org $10000-20
absirq:	pha
	txa
	pha
	tya
	pha
	jsr	irq
	pla
	tay
	pla
	tax
	pla
arti:	rti
	.word	arti
	.word	arti
	.out	.sprintf("$FFFE=$%04x",*)
	.assert *=$FFFE, error, "missed IRQ vector in irqCPU"
	.word	absirq
absirq_end:
absirq_length=absirq_end-absirq
	.reloc
.endproc

	.code
_theatre_wait_frame:
	lda	_theatre_frame_counter ; LO
@loop:	cmp	_theatre_frame_counter
	beq	@loop
	rts

	.code
_theatre_init:
	;; Calling convention: https://cc65.github.io/doc/cc65-intern.html#ss2.1
	;; A=songnumber
	;; muzak_init
	;; muzak_play
	sei
	pha			; Songnumber
	jsr	popax
	sta	muzak_play_ptr
	stx	muzak_play_ptr+1
	jsr	popax
	sta	muzak_init_ptr
	stx	muzak_init_ptr+1
	pla			; Songnumber
	jsr	$2020
	muzak_init_ptr=*-2
	lda	#$ff
	sta	$d015		; Enable all sprites.
	sta	_theatre_frame_counter ; Clear frame counter, frames will start with zero.
	sta	_theatre_frame_counter+1
	jsr	_disable_cia_irq
	SwitchScreenAndChargenAddress	THEATRE_TEXT, THEATRE_FONT
	lda	#>(THEATRE_TEXT)
	sta	648		; Set high byte of text screen
	EnableIRQatRasterline RASTERLINE_FOR_INTERRUPT
	SetIRQ314Pointer irq314
	SmallMemCpy	irqCPU,irqCPU::absirq_length,irqCPU::absirq
	SetIRQCPUPointer irqCPU::absirq
	rts

_act_init:
	sta	@params
	stx	@params+1
	jsr	popa		; Border
	sta	@params+3
	jsr	popa		; Background
	sta	@params+2
	jsr	popax		; Pointer to animation
	sta	animation_ptr
	stx	animation_ptr+1
	jsr	popa
	sta	@params+4
	jsr	act_init
@params = *
	.res	5
	cli
	rts


.proc	copy_font0800
	ldx	#0
loop:
	.repeat 8,I
	 lda	$0800+$100*I,x
	 sta	CHARGEN_DESTINATION+$100*I,x
	.endrepeat
	dex
	bne	loop
	rts
.endproc


;;; uint16 pointer to pu data, 1-2
;;; background, 3
;;; border, 4
;;; flags, 5
.proc act_init
	GetReturnAddrIntoPointer ptr1
	PointerAdjustedToStack	ptr1, 5
	lda	$d011		; Turn screen off
	and	#%01101111
	sta	$d011
	jsr	busywait_frame_mp
	;; Remove sprites from screen.
	ldx	#$10
	lda	#0
l1:	sta	$d000,x
	dex
	bpl	l1
	ldy	#4
	lda	(ptr1),y
	sta	$d020
	dey
	lda	(ptr1),y
	sta	$d021
	dey
	lda	(ptr1),y	; HI of compressed data
	tax
	dey
	lda	1		; store memory configuration
	pha
	memoryconfig_ram
	lda	(ptr1),y	; LO of compressed data
	jsr	pudecrunch_default
	ldy	#5		; Flags
	lda	(ptr1),y
	sta	flags_passed
	and	#THEATRE_COPY_CHARGEN
	beq	no_copy_chargen
	jsr	copy_chargen
no_copy_chargen:
	lda	flags_passed
	and	#THEATRE_COPY_FONT0800
	beq	no_copy_font0800
	jsr	copy_font0800
no_copy_font0800:
	memoryconfig_io
	;; Bitmap mode?
	lda	flags_passed
	and	#THEATRE_BITMAP_MODE	; Bit #1
	asl
	asl
	asl
	asl			; now Bit #5
	sta	tmp1
	lda	$d018
	and	#%11110111
	sta	$d018
	lda	tmp1
	beq	no_set_gfx_to_e000
	lda	$d018
	ora	#%00001000
	sta	$d018
no_set_gfx_to_e000:
	lda	$d011
	;;        76543210
	and	#%01011111
	ora	tmp1
	sta	$d011
	;; Multicolour mode?
	lda	flags_passed
	and	#4		; Bit #2
	asl
	asl
	sta	tmp1		; Bit #4
	lda	$d016
	and	#%11101111
	ora	tmp1
	sta	$d016
	;; Copy the colours or blank.
	lda	flags_passed
	and	#THEATRE_ACT_BLANK
	if	eq
	 Memcpy1KViaPtr	THEATRE_COLR, $d800
	else
	 lda	$d021
	 jsr	_fill_colour_ram
	 ldax	#THEATRE_TEXT
	 ldy	#' '
	 jsr	fill_1000_bytes
	endif
	pla
	sta	1		; restore memory configuration
	rts
	.pushseg
	.bss
flags_passed:	.res	1
	.popseg
.endproc

;;; Y=number of bytes pushed on stack, see https://cc65.github.io/doc/cc65-intern.html#toc1.2. Last will be a NULL.
;;; Parameters are put left to right on the C stack. We will have to get Y/2 pointers and push them on the CPU stack until we reach the configuration pointer. Then we can store the configuration pointer and pop the function pointers from the CPU stack in order to call the functions.
;;; stack: function pointers
.proc	_scene_init
	.pushseg
	sty	fptrMaxIDX	; Store number of bytes on stack
	lsr	fptrMaxIDX	; Divide bytes on stack by two, aka number of functions.
pushloop:
	jsr	popax		; Get pointer into AX
	pushax			; Push on CPU stack!
	dec	fptrMaxIDX
	bne	pushloop
	pullax			; Get leftmost parameter (pointer) of function.
	stax	ptr1		; This is the configuration pointer, store it.
	;; Get ISR from config
	sei
	ldy	#TheatreSceneConfiguration::isr
	lda	(ptr1),y
	sta	_theatre_irq_isr_vector
	iny
	lda	(ptr1),y
	sta	_theatre_irq_isr_vector+1
	cli
	;; Get the interpicture function.
	ldy	#TheatreSceneConfiguration::interpicture
	lda	(ptr1),y
	sta	interpicture
	iny
	lda	(ptr1),y
	sta	interpicture+1
	;; Now play the pictures.
	lda	$d011		; Turn screen on
	ora	#%00010000
	and	#%01111111
	sta	$d011
	;; Now loop through the pictures until the NULL pointer.
picture_loop:
	pullax			; Get pointer from CPU stack
	stax	fptr		; Store into the pointer for JSR
	ora	fptr+1		; A or fptr+1 (aka X) in order to test for NULL pointer
	beq	out
	jsr	$2020		; dummy variable
	fptr=*-2
	;; Always wait a frame between pictures.
	jsr	_theatre_wait_frame
	PointerTestAndCall interpicture
	jmp	picture_loop
out:	rts
	.bss
interpicture:	.res	2
fptrMaxIDX:	.res	1
	.popseg
.endproc
