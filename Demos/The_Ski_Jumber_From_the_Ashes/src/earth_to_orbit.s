	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/theatre/consts.i"
	.include	"LAMAlib-macros16.inc"
	.include	"resident.i"
	.include	"globals.i"
	.include	"t7d/pseudo/loadstorereg.i"
	.include	"dload-labels.inc"
	.macpack	longbranch

	.export	_copy_major_tom_to_1000

ANIMATIONSCREEN=MEMMAP_gfxarea

	.rodata
nextpartname:	.byte	"hyperspace.2200"
nextpartname_end:

_copy_major_tom_to_1000:
	.incbin	"copy_major_tom_to_1000.e000.pu",2


	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	jsr	Start
	jmp	nextpart
Start:
	;; Go to C code.

	.code
	.include	"temporary.earth2orbit.petscii.inc"
	.export	_animation_base_addr=animation_petscii_jumptable

	.segment	"LOWCODE"
cassette:	
	.org	$33c
	jmp	cassette_main
	;; IRQ here...
	jsr     call_yielding_function
	rts

yielding_regsave_a:     .res    1
yielding_regsave_x:     .byte	39
yielding_regsave_y:     .byte	0
yielding_pc_hi: .byte	>(yielding-1)
yielding_pc_lo: .byte	<(yielding-1)

.proc call_yielding_function
        jsr     actual
        ;; Only return here if yielding function ended in a RTS.
        lda     #>(therts-1)    ; Call will point to a RTS
        sta     yielding_pc_hi
        lda     #<(therts-1)
        sta     yielding_pc_lo
therts: rts
actual: lda     yielding_pc_hi
        pha
        lda     yielding_pc_lo
        pha
        lda     yielding_regsave_a
        ldx     yielding_regsave_x
        ldy     yielding_regsave_y
        rts                     ; actuall a jump to yielding_pc
.endproc

.proc   call_yield
        sta     yielding_regsave_a
        stx     yielding_regsave_x
        sty     yielding_regsave_y
        pla
        sta     yielding_pc_lo
        pla
        sta     yielding_pc_hi
        ;; Do not return to the point where the function called call_yield.
        pla
        pla
        ;; Return to the function which called call_yielding_function!
        rts
.endproc

.proc	draw_it
	.repeat	25,I
	pha
	sta	MEMMAP_gfxarea+40*I,x
	sta	MEMMAP_gfxarea+40*I,y
	lda	#$f
	sta	$d800+40*I,x
	sta	$d800+40*I,y
	pla
	.endrepeat
	rts
.endproc

yielding:
	;; First time, draw only...
	lda	#$e6
	jsr	draw_it
	jsr     call_yield
	jsr     call_yield
	jsr     call_yield
loop:	;; Now kill and draw
	lda	#' '
	jsr	draw_it
	dex
	DEXPTR=*-1
	iny
	DEYPTR=*-1
	lda	#$e6
	jsr	draw_it
	jsr     call_yield
	jsr     call_yield
	jsr     call_yield
	jsr     call_yield
	cpx	#0
	bne	noxeor
	lda	DEXPTR
	eor	#$CA^$E8
	sta	DEXPTR
noxeor:	cpx	#39
	bne	noxeor2
	lda	DEXPTR
	eor	#$CA^$E8
	sta	DEXPTR
noxeor2:
	cpy	#0
	bne	noyeor
	lda	DEYPTR
	eor	#$88^$C8
	sta	DEYPTR
noyeor:	cpy	#39
	bne	noyeor2
	lda	DEYPTR
	eor	#$88^$C8
	sta	DEYPTR
noyeor2:
	jne	loop
	rts
cassette_main:
	lda	#0
	sta	LdZp_PresetAE
	Load16	x,y,#nextpartname
	lda	#nextpartname_end-nextpartname
	jsr	LdLoc
	bcc	okload
	lda	#2
	sta	$d020
	jmp	*
okload:	jsr	RESIDENT_set_default_irq
	bit	Control_single_mode
	bmi	*
	jmp	MEMMAP_democode
	
	.reloc

.proc	nextpart
	lda	#$c8
	sta	$d016
	ldax	#cassette
	ldy	#RESIDENT_datasette_jmp_33c|RESIDENT_datasette_irq_33f
	jmp	RESIDENT_copy_datasette
.endproc

