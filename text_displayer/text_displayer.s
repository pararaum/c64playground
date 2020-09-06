;;; cl65 -m /dev/stderr  -C muzak.cfg text_displayer.s
stubpos	= $32e
charsetaddr = $0800
tedi_screen_ram = $0400

	.import	muzak_init
	.import	set_irq
	.import vic_init

	.include	"text_output.i"

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
	jsr	prepare_basic_warmstart
	jsr	tedi_init
	tedi_inc_n_call	"hill.seq",0
	tedi_inc_n_call	"klammeraffe.seq",0
	;; https://csdb.dk/release/?id=167761
	;; imported via https://www.lvllvl.com/
	tedi_inc_n_call	"PETbench.seq",1
	;; https://csdb.dk/release/?id=136635
	tedi_inc_n_call	"retro-z.seq",1
final_code:
	lda	#0
	tax
@l2:	sta	$0800,x
	dex
	bne	@l2
	brk

prepare_basic_warmstart:
	lda	$300
	sta	basic_warmstart_ptr
	lda	$300+1
	sta	basic_warmstart_ptr+1
	lda	#<basic_warmstart
	sta	$0300
	lda	#>basic_warmstart
	sta	$0301
	rts
	
basic_warmstart:
	ldx	#0
@l1:	lda	@text,x
	beq	@out
	jsr	$ffd2
	inx
	bne	@l1
	;; For colour codes, see https://www.c64-wiki.com/wiki/PETSCII_Codes_in_Listings.
@text:	.byte	$9F,"CODE: PARARAUM/T7D",$D,"MUSIC: HANS JUERGEN EHRENTRAUT",$D
	.byte	"GFX: PARARAUM, DR TERRORZ, ZIILI"
	.byte	$9A,0
@out:	ldx	#$80
	lda	basic_warmstart_ptr
	sta	$0300
	lda	basic_warmstart_ptr+1
	sta	$0300+1
	jmp	$FFFF
	basic_warmstart_ptr = *-2
