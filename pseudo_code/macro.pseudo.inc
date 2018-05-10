;;; -*- mode: asm -*-

P_and:	MACRO src, dst
	lda src
	and dst
	sta dst
	lda src+1
	and dst+1
	sta dst+1
	ENDM

P_eor:	MACRO src, dst
	lda src
	eor dst
	sta dst
	lda src+1
	eor dst+1
	sta dst+1
	ENDM

P_load:	MACRO src, dst
	ldy #0
	lda (src),y
	sta dst
	iny
	lda (src),y	
	sta dst+1
	ENDM
	
P_loadi:	MACRO reg, val
	lda #<(val)
	sta reg
	lda #>(val)
	sta reg+1
	ENDM

P_pull:	MACRO reg
	pla
	sta reg
	pla
	sta reg+1
	ENDM

P_push:	MACRO reg
	lda reg+1
	pha
	lda reg
	pha
	ENDM

;;; Push a 16-bit value immediate on the stack, can be used for jump tables
P_pushi:	MACRO data
	lda #>(data)
	pha
	lda #<(data)
	pha
	ENDM

P_shiftl:	MACRO reg
	asl reg
	rol reg+1
	ENDM

P_shiftr:	MACRO reg
	lsr reg+1
	ror reg
	ENDM

P_store:	MACRO src, dst
	ldy #0
	lda src
	sta (dst),y
	lda src+1
	iny
	sta (dst),y	
	ENDM

P_storeb:	MACRO src, dst
	ldy #0
	lda src
	sta (dst),y
	ENDM

P_sub:	MACRO src, dst
	sec
	lda src
	sbc dst
	sta dst
	lda src+1
	sbc dst+1
	sta dst+1
	ENDM

P_transfer:	MACRO src, dst
	lda src
	sta dst
	lda src+1
	sta dst+1
	ENDM

P_add:	MACRO src, dst
	clc
	lda src
	adc dst
	sta dst
	lda src+1
	adc dst+1
	sta dst+1
	ENDM

P_branchNZ:	MACRO reg, disp
	lda reg+1
	bne disp
	lda reg
	bne disp
	ENDM

P_branchZ:	MACRO reg, disp
	lda reg+1
	bne .out
	lda reg
	beq .disp
.out:
	ENDM

P_exit:	MACRO
	nop
	ENDM
