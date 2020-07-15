;;; cl65 -m /dev/stderr  -C muzak.cfg text_displayer.s
stubpos	= $32e
charsetaddr = $0800

	.import	muzak_init
	.import	set_irq
	.import vic_init
	.export main
	.export charsetdata
	.export charsetaddr


	.segment "LOADADDR"
	.word	$0801
	

	.segment "EXEHDR"
	.word	end_of_basic
	.word	7
	.byte	$9e,"(2064)"
end_of_basic:
	.byte	0,0,0
	nop
	sei
	lda	#0
	jsr	muzak_init
	jsr	vic_init
	jsr	set_irq
	jmp	copy_stub
	.segment "EXEHDR"
copy_stub:
	ldx	#0
@l1:	lda	stub,x
	sta	stubpos,x
	inx
	cpx	#<(orgstubend-orgstub)
	bne	@l1
	jmp	stubpos
stub:
	.org	stubpos
orgstub:
	ldx	#0
	ldy	#2-1
@l1:	lda	charsetdata,x
@l2:	sta	charsetaddr,x
	inx
	bne	@l1
	inc	@l1+2		; Increment high byte source.
	inc	@l2+2		; Increment high byte destination.
	dey
	bpl	@l1
	jmp	main
orgstubend:
	.reloc
	
charsetdata:
	.incbin "charset"

	
	.data
	.byte "the 7th division"


	.bss
counter:	.res	1


	.code
main:
	nop
@l1:
	jsr	$ffe4		; GETIN
	beq	@l1
	cmp	#3		; RUN/STOP
	beq	@out
	jsr	$ffd2
	jmp	main
@out:
	lda	#0
	ldx	#0
@l2:	sta	$0800,x
	dex
	bne	@l2
	brk
	
	
