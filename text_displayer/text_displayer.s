;;; cl65 -m /dev/stderr  -C muzak.cfg text_displayer.s
stubpos	= $32e
charsetaddr = $0800
tedi_screen_ram = $0400

	.import	muzak_init
	.import	set_irq
	.import vic_init
	.import	tedi_chrout
	.import	tedi_init
	.import	tedi_output_text

	.export main
	.export charsetdata
	.export charsetaddr
	.export tedi_screen_ram

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
txt_1:
	.incbin	"doc1.seq"
	.byte	0
txt_2:
	.incbin	"doc2.seq"
	.byte	0
txt_3:
	.incbin	"doc3.seq"
	.byte	0


	.code
wfk:
@l1:
	jsr	$ffe4		; GETIN
	beq	@l1
	cmp	#3		; RUN/STOP
	beq	@out
	;; 	jsr	$ffd2
	cmp	#$20
	beq	@ret
	cmp	#$d
	beq	@ret
	jmp	@l1
@out:	jmp final_code
@ret:	rts

main:
	jsr	tedi_init
	lda	#<txt_1
	ldx	#>txt_1
	jsr	tedi_output_text
	jsr	wfk
	lda	#<txt_2
	ldx	#>txt_2
	jsr	tedi_output_text
	jsr	wfk
	lda	#<txt_3
	ldx	#>txt_3
	jsr	tedi_output_text
	jsr	wfk
final_code:
	lda	#0
	ldx	#0
@l2:	sta	$0800,x
	dex
	bne	@l2
	brk

