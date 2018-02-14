	.word $0801
	* = $0801
	cinv = $0314
	raster = $f9
	
basic:	.(
	.word L1
	.word main
	.byte $9e
	.asc "(",$c6,"(",$22
	.byte ((main>>6)&$3f)+$20
	.asc $22,")",$ab,"32)",$ac,"64",$aa,$c6,"(",$22
	.byte ((main)&$3f)+$20
	.asc $22,")",$ab,"32"
	.asc 0
L1:	.word end_of_basic
	.word L1
	.byte $8f,$20
	.asc "ANOTHER VISITOR STAY A WHILE...",0
end_of_basic:
	.word 0,0
	.)

main:	jmp install
	jmp fillscreen
	jmp deinstall

oldirq:	.word $fffe

install:        ; install the raster routine
	nop
checkirq:
	lda cinv      ; check the original IRQ vector
	ldx cinv+1    ; (to avoid multiple installation)
	cmp #<irqvector
	bne irqinit
	cpx #>irqvector
	brk
irqinit:
	sei
	sta oldirq    ; store the old IRQ vector
	stx oldirq+1
	lda #<irqvector
	ldx #>irqvector
	sta cinv      ; set the new interrupt vector
	stx cinv+1
	;; Set multicolour mode
	lda #$1b
	ora #$40		;ECM
	sta $d011     ; set the raster interrupt location
	lda #raster
	sta $d012
	lda #$7f
	sta $dc0d     ; disable timer interrupts
	sta $dd0d
	lda #%00000001		;enable raster IRQ
	sta $d01a     ; enable raster interrupt
	lda $dc0d     ; acknowledge CIA interrupts
	lsr $d019     ; and video interrupts
	ldy #$ff
	sty $d015     ; turn on all sprites
	jsr fillscreen
	cli
	rts

fillscreen:	.(
	ldx #$00
l1:	txa
	sta $0400,x
	inx
	bne l1
	rts
	.)
	
;;; Deinstallation
deinstall:	brk

irqvector:	.(
	inc $d019	; Acknowledge interrupt.
	sta $02
	stx $03
	sty $04
	inc $d020
	lda #%11110111
	and $d011
	sta $d011
wait1:	dec $39ff
	inc $39ff
	bit $d011
	bpl wait1
	ora #%00001000
	sta $d011
	dec $d020
	lda $02
	ldx $03
	ldy $04
	jmp (oldirq)
	.)
