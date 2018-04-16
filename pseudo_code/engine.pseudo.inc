;;; -*- mode: asm -*-

PUSHPSEUDOPC:	MACRO
	lda PSEUDOPC
	pha
	lda PSEUDOPC+1
	pha
	ENDM
P_add:	MACRO src, dst
	.byte PSEUDO_CODE_add, src, dst
	ENDM
P_branchNZ:	MACRO reg, disp
 	.byte PSEUDO_CODE_branchNZ, reg, disp-*
	ENDM
P_exit:	MACRO
	.byte PSEUDO_CODE_exit
	ENDM
P_loadi:	MACRO reg, val
	.byte PSEUDO_CODE_loadi, reg
	.word val
	ENDM
P_store:	MACRO src, dst
	.byte PSEUDO_CODE_store, src, dst
	ENDM
P_sub:	MACRO src, dst
	.byte PSEUDO_CODE_sub, src, dst
	ENDM
P_transfer:	MACRO src, dst
	.byte PSEUDO_CODE_transfer, src, dst
	ENDM

PSEUDO_CODE_exit = $00			;Exit P-Code interpreter
PSEUDO_CODE_loadi = $01			;Load immediate (reg, val)
PSEUDO_CODE_transfer = $02		;Transfer (reg, reg)
PSEUDO_CODE_sub = $03			;Subtract (reg0, reg1): reg1 ≔ reg1-reg0
PSEUDO_CODE_add = $04			;Add (reg0, reg1): reg1 ≔ reg1+reg0
PSEUDO_CODE_branchZ = $05		;Branch on zero (reg0, displacement): jump if reg0 == 0
PSEUDO_CODE_branchNZ = $06		;Branch on zero (reg0, displacement): jump if reg0 != 0
PSEUDO_CODE_store = $07			;Store register, indirect (reg0, [reg1]): reg0 → [reg1]
;;; ↑↓ Keep these two in sync or you will get into serious trouble... ☠

jtable:	.word TABexit, TABloadImmediate, TABtransfer, TABsubtract, TABadd, TABillegal, TABbranchNZ, TABstore
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal
	.word TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal, TABillegal


pseudo:	tsx
	lda $0101,x
	sta PSEUDOPC
	lda $0102,x
	sta PSEUDOPC+1
.loop:	ldy #1	
.ptr:	lda (PSEUDOPC),y		;next opcode
	bpl *+3				;Hang if too many.
	.byte $d2
	asl
	tax
	lda jtable,x
	sta .jump+1
	lda jtable+1,x
	sta .jump+2
	iny
.jump:	jsr $ffff
	;; Add Y to the PSEUDOPC; this will be a little slower but we are not restricted to 255 bytes.
	tya			;Move currently used bytes to A
	clc
	adc PSEUDOPC
	sta PSEUDOPC
	bcc .no_carry
	inc PSEUDOPC+1
.no_carry:	jmp .loop


TABexit:	tsx
	inx			;Remove return address of .jump.
	inx
	lda PSEUDOPC
	clc
	adc #$01
	sta $0101,x
	lda PSEUDOPC+1
	adc #0			;Add carry, as sta, lda do not change the flag.
	sta $0102,x
	txs
	rts


TABstore:	lda (PSEUDOPC),y ;register to store
	iny
	tax			; X is register to store
	lda (PSEUDOPC),y
	sta .dst0+1		; register for indirection
	sta .dst1+1		; register for indirection
	ldy #0
	lda $00,x
.dst0:	sta ($00),y
	lda $01,x
	iny
.dst1:	sta ($00),y
	ldy #3			;opcode, reg0, reg1
	rts


TABbranchNZ:	lda (PSEUDOPC),y
	tax			;Registernumber in X
	lda $01,x
	bne .branch
	lda $00,x
	bne .branch
	ldy #3		;opcode, reg0, displacement
	rts
.branch:	iny		;displacement offset
	ldx #0
	lda (PSEUDOPC),y	;displacement
	bpl .l1
	dex			;for sign extension
.l1	clc
	adc PSEUDOPC
	sta PSEUDOPC
	txa
	adc PSEUDOPC+1
	sta PSEUDOPC+1
	ldy #0
	rts 


TABadd:	lda (PSEUDOPC),y
	tax			; X is source.
	iny
	lda (PSEUDOPC),y	; Y is target.
	tay
	clc
	lda $00,x
	adc $00,y
	sta $00,y
	lda $01,x
	adc $01,y
	sta $01,y
	ldy #3			;Three bytes were used (opcode, reg0, reg1).
	rts

TABsubtract:	lda (PSEUDOPC),y
	tax			; X is source.
	iny
	lda (PSEUDOPC),y	; Y is target.
	tay
	sec
	lda $00,x
	sbc $00,y
	sta $00,y
	lda $01,x
	sbc $01,y
	sta $01,y
	ldy #3			;Three bytes were used (opcode, reg0, reg1).
	rts

TABloadImmediate:	lda (PSEUDOPC),y
	tax
	iny
	lda (PSEUDOPC),y
	sta $00,x
	iny
	inx
	lda (PSEUDOPC),y
	sta $00,x
	rts

TABtransfer:	lda (PSEUDOPC),y
	sta .src+1
	iny
	lda (PSEUDOPC),y
	sta .dst+1
	ldx #$01
.src:	lda $00,x
.dst:	sta $00,x
	dex
	bpl .src
	rts
	
TABillegal:	lda #$02
	sta $d020
	sta $d021
.l1:	inc $d020
	jmp .l1
