
	.export	stubJUMPVIARTSLO_offset = JUMPVIARTSLO-$0801
	.export	stubJUMPVIARTSHI_offset = JUMPVIARTSHI-$0801
	.export	stubMVDEST_offset = MVDEST-$0801
	.export	stubMVELEN_offset = MVELEN-$0801

srcptr=$5C			; Source pointer in ZP
destptr=$5E			; Destination pointer in ZP
STUBDESTINATION=$100		; Where to put the stub into low memory.

	.segment	"EXEHDR"
	.incbin	"vcclogo.prg",2
	jmp	main
	
	;; Copy routine: L.A. Leventhal, W. Saville, "6502 Assembly Language Subroutines", Osborne/McGraw-Hill, 1982, p. 201.

	.data
	;; Make sure this is the last data!
data_begin:
data_end:

	.rodata
MVSRCE:	.word	data_begin
MVDEST:	.word	$0801
MVELEN:	.word	data_end-data_begin

	.rodata
stackstub:
	.org	STUBDESTINATION
	lda	MVELEN		; Get LO of move length, length of last page.
	pha			; And move it onto the stack.
	;; Prepare the pointers.
	lda	MVSRCE
	sta	srcptr
	lda	MVSRCE+1
	sta	srcptr+1
	lda	MVDEST
	sta	destptr
	lda	MVDEST+1
	sta	destptr+1
	;; Copyloop
	ldy	#0
	ldx	MVELEN+1	; Number of full pages.
	beq	partial_page
sl1:	lda	(srcptr),y
	sta	(destptr),y
	iny
	bne	sl1
	inc	srcptr+1	; Advance to next page of source...
	inc	destptr+1	; ...and destination.
	dex			; Decrement page count.
	bne	sl1
partial_page:
	pla			; Get length of last page from stack.
	tax			; Move to X.
sl2:	lda	(srcptr),y
	sta	(destptr),y
	iny			; Next byte, moving upward in memory.
	dex			; Decrement count.
	bne	sl2
	;; Restore memory and jump to program.
	pla
	sta	1
	cli
	rts			; Actually a JMP!
	.reloc
stackstub_end:
stackstub_len=stackstub_end-stackstub

	.code
main:	sei
	;; Prepare stack for RTS
	lda	#>(64738-1)
	JUMPVIARTSHI=*-1
	pha
	lda	#<(64738-1)
	JUMPVIARTSLO=*-1
	pha
	;; Get old memory configuration.
	lda	1
	pha
	lda	#$34
	sta	1		; Set to all RAM configuration.
	;; Copy the stub into stack.
	ldx	#0
l1:	lda	stackstub,x
	sta	STUBDESTINATION,x
	inx
	cpx	#stackstub_len
	bne	l1
	jmp	STUBDESTINATION

