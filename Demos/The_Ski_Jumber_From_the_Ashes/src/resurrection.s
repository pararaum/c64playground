	.include	"t7d/memoryconfig.i"
	.include	"t7d/memoryfunctions.i"
	.include	"LAMAlib-macros16.inc"
	.include	"resident.i"
	.include	"globals.i"

BUFFER=$200

	.segment	"ZEROPAGE"
cptr:	.res	2		; Pointer for cassette routine

	.rodata
nextpartname:	.asciiz	"slow-start.Z"

	.segment	"EXEHDR"
	.export	__EXEHDR__:absolute=1
	jsr	init
	sei
	jsr	Start
	jmp	nextpart
Start:
	;; Go to C code.

	.segment	"LOWCODE"
init:
	CopyFileIntoMemoryDown $e000,"from_the_ashes.e000.prg",2
	sei
	memoryconfig_io
	CopyFileIntoMemoryDown	$1000,"assets/terminator_music.prg",2
	lda	#1
	jsr	$1000
	cli
	rts

nextpart:
	ldax	#cassette
	ldy	#RESIDENT_datasette_jmp_33c|RESIDENT_datasette_irq_33f
	jmp	RESIDENT_copy_datasette

cassette:
	.org	$33c
	jmp	cassette_main
	ldy	#40-1		; We keep column in Y!!!
columnloop:
	ldx	positions,y	; Get position aka row, so X=row, now.
	bmi	nocolrestore	; Outside skip this step.
	lda	textLO,x	; LO of row address...
	sta	cptr
	lda	textHI,x	; ...and HI of row address...
	sta	cptr+1		; ...are put into cptr.
	lda	BUFFER,y	; And restore the previously store character.
	sta	(cptr),y
nocolrestore:
	inx			; Increment to next row!
	txa
	sta	positions,y
	bmi	nocolstore	; Position still outside? So, no storage!
	lda	textLO,x	; LO of row address...
	sta	cptr
	lda	textHI,x	; ...and HI of row address...
	sta	cptr+1		; ...are put into cptr.
	lda	(cptr),y	; Get old character
	sta	BUFFER,y	; And store in a safe place.
	lda	#$E0
	sta	(cptr),y	; Now store the snow flake.
nocolstore:
	lda	positions,y	; Get position aka row, so A=row, now.
	bmi	next
	cmp	#2		; Second row?
	bcc	next		; ≤ 2
	cmp	finalrow,y
	bcc	next
	lda	#$FF		; Reset character.
	sta	positions,y
	lda	#$e0
	sta	(cptr),y	; Overwrite character.
	lda	colHI,x
	sta	cptr+1
	lda	#1
	sta	(cptr),y
	lda	finalrow,y
	sec
	sbc	#1		; Reduce by one.
	bmi	fini
	sta	finalrow,y
next:	dey
	bpl	columnloop
	rts
fini:	ldx	#40-1
finiloop:
	lda	#$e0
	sta	MEMMAP_gfxarea,x
	sta	MEMMAP_gfxarea+40,x
	sta	MEMMAP_gfxarea+40*2,x
	sta	MEMMAP_gfxarea+40*3,x
	lda	#1
	sta	$d800,x
	sta	$d800+40,x
	sta	$d800+40*2,x
	sta	$d800+40*3,x
	dex
	bpl	finiloop
	jmp	RESIDENT_set_default_irq

finalrow:	.res	40, 24	; Final row to be reached.
positions:
	.byte	252, 248, 245, 246, 252, 246, 249, 251, 250, 250, 245, 253, 254, 254, 250, 247, 249, 245, 247, 253, 251, 244, 252, 254, 254, 248, 248, 252, 246, 247, 250, 247, 247, 253, 254, 254, 252, 244, 246, 250
textLO:
	.repeat	25,I
	.byte	<(MEMMAP_gfxarea+I*40)
	.endrepeat
textHI:
	.repeat	25,I
	.byte	>(MEMMAP_gfxarea+I*40)
	.endrepeat
colHI:
	.repeat	25,I
	.byte	>($D800+I*40)
	.endrepeat

cassette_main:
	ldax	#nextpartname
	ldy	#RESIDENT_load8000|RESIDENT_pucrunch|RESIDENT_keep_irq
	jsr	RESIDENT_load_nextpart
	jmp	RESIDENT_decrunch_last_loaded
	.reloc
