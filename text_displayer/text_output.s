;;; Screen output functions as the kernal functions are not very useful to us. 
	
	CURRENT_COLOR = 646

	.export	tedi_chrout
	.export	tedi_init
	.export	tedi_output_text
	.import	tedi_screen_ram	; Where is the screen ram?

	.zeropage
tedi_scrptr:	.res	2
	;; This module uses a pointer in the zeropage.
	.zeropage
otxtptr:	.res	2	; Pointer to the next character for text output.

	.bss
reverse_mode_bit:	.res	1

	.data
tab_petscii2screencode:
										;PETSCII RANGE
	.byte $80,$81,$82,$83,$84,$85,$86,$87, $88,$89,$8a,$8b,$8c,$8d,$8e,$8f	;$00-...
	.byte $90,$91,$92,$93,$94,$95,$96,$97, $98,$99,$9a,$9b,$9c,$9d,$9e,$9f	;...-$1f
	.byte $20,$21,$22,$23,$24,$25,$26,$27, $28,$29,$2a,$2b,$2c,$2d,$2e,$2f	;$20-...
 	.byte $30,$31,$32,$33,$34,$35,$36,$37, $38,$39,$3a,$3b,$3c,$3d,$3e,$3f	;...-$3f
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$40-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$1b,$1c,$1d,$1e,$1f	;...-$5f
	.byte $40,$41,$42,$43,$44,$45,$46,$47, $48,$49,$4a,$4b,$4c,$4d,$4e,$4f	;$60-...
 	.byte $50,$51,$52,$53,$54,$55,$56,$57, $58,$59,$5a,$5b,$5c,$5d,$5e,$5f	;...-$7f
	.byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7, $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf	;$80-...
 	.byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7, $d8,$d9,$da,$db,$dc,$dd,$de,$df	;...-$9f
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$a0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$7f	;...-$bf
	.byte $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	;$c0-...
 	.byte $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1a,$1b,$1c,$1d,$1e,$1f	;...-$df
	.byte $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6a,$6b,$6c,$6d,$6e,$6f	;$e0-...
 	.byte $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7a,$7b,$7c,$7d,$7e,$5e	;...-$ff
	
	.code
tedi_init:
	lda	#<tedi_screen_ram
	sta	tedi_scrptr
	lda	#>tedi_screen_ram
	sta	tedi_scrptr+1
	lda	#0
	sta	reverse_mode_bit
	rts

	.code
tedi_chrout:
	pha
	stx	@regx+1
	sty	@regy+1
	pha
	and	#%01111111	; Ignore highest bit.
	cmp	#$20		; Check for control code.
	pla
	bcs	@nocontrol	; No control code.
	jsr	tedi_handle_control
	jmp	@ok
@nocontrol:
	tax
	lda	tab_petscii2screencode,x
	ora	reverse_mode_bit
	ldy	#0
	sta	(tedi_scrptr),y
	lda	tedi_scrptr	; Low byte of current pointer
	sta	@stacol+1
	lda	tedi_scrptr+1	; high byte of current pointer
	clc
	adc	#$d8-$04	; Adjust to colour information.
	sta	@stacol+2
	lda	CURRENT_COLOR
@stacol:	sta	$ffff
	inc	tedi_scrptr
	bne	@nohi
	inc	tedi_scrptr+1
@nohi:
	lda	tedi_scrptr+1
	cmp	#8
	bne	@ok
	lda	#4
	sta	tedi_scrptr+1
@ok:
@regx:	ldx	#00	
@regy:	ldy	#00	
	pla
	rts

	.code
tedi_handle_control:
	cmp	#$13		; home
	bne	@l2
	jmp	tedi_init
@l2:	cmp	#$93		; clear
	bne	@l1
	jsr	tedi_init
	lda	#$20
	ldx 	#0
@loop1:	sta	tedi_screen_ram,x
	sta	tedi_screen_ram+$0100,x
	sta	tedi_screen_ram+$0200,x
	sta	tedi_screen_ram+$0300-24,x
	dex
	bne	@loop1
@l1:	cmp	#$90		; black colour
	bne	@nc0
	lda	#0
	sta	CURRENT_COLOR
	jmp	@out
@nc0:	cmp	#$5		; white colour
	bne	@nc1
	lda	#1
	sta	CURRENT_COLOR
	jmp	@out
@nc1:	cmp	#$1c		; red colour
	bne	@nc2
	lda	#2
	sta	CURRENT_COLOR
	jmp	@out
@nc2:	cmp	#$9f		; cyan
	bne	@nc3
	lda	#3
	sta	CURRENT_COLOR
	jmp	@out
@nc3:	cmp	#$9c		; pruple
	bne	@nc4
	lda	#4
	sta	CURRENT_COLOR
	jmp	@out
@nc4:	cmp	#$1e		; green colour
	bne	@nc5
	lda	#5
	sta	CURRENT_COLOR
	jmp	@out
@nc5:	cmp	#$1f		; blue
	bne	@nc6
	lda	#6
	sta	CURRENT_COLOR
	jmp	@out
@nc6:	cmp	#$9e		; yellow
	bne	@nc7
	lda	#7
	sta	CURRENT_COLOR
	jmp	@out
@nc7:	cmp	#$81		; orange
	bne	@nc8
	lda	#8
	sta	CURRENT_COLOR
	jmp	@out
@nc8:	cmp	#$95		; brown
	bne	@nc9
	lda	#9
	sta	CURRENT_COLOR
	jmp	@out
@nc9:	cmp	#$96		; light red, pink
	bne	@nc10
	lda	#10
	sta	CURRENT_COLOR
	jmp	@out
@nc10:	cmp	#$97		; dark gray
	bne	@nc11
	lda	#11
	sta	CURRENT_COLOR
	jmp	@out
@nc11:	cmp	#$98		; gray
	bne	@nc12
	lda	#12
	sta	CURRENT_COLOR
	jmp	@out
@nc12:	cmp	#$99		; light green
	bne	@nc13
	lda	#13
	sta	CURRENT_COLOR
	jmp	@out
@nc13:	cmp	#$9a		; light blue
	bne	@nc14
	lda	#14
	sta	CURRENT_COLOR
	jmp	@out
@nc14:	cmp	#$9b		; light gray
	bne	@nc15
	lda	#15
	sta	CURRENT_COLOR
@nc15:	cmp	#$12		; reverse on
	bne	@nrevon
	lda	#$80
	sta	reverse_mode_bit
@nrevon: cmp	#$92		; reverse off
	bne	@nrevoff
	lda	#$00
	sta	reverse_mode_bit
@nrevoff:
@out:
	;; $9d=cursor left
	;; $1d=cursor right
	;; $91=cursor up
	;; $11=cursor down
	rts

	.code
;;; Output a text string on the screen in PETSCII. Only a few control codes are implemented. The PETSCII code \0 marks the End Of String.
;;; Input: A/X=pointer to string
;;; Output: -
;;; Modifies A,X(?),Y
tedi_output_text:
	;; Set pointer to string.
	sta	otxtptr
	stx	otxtptr+1
	ldy	#0		; For indirect access.
@l1:	lda	(otxtptr),y
	beq	@out		; End of String
	jsr	tedi_chrout
	inc	otxtptr
	bne	@l1
	inc	otxtptr+1
	bne	@l1
@out:
	rts
