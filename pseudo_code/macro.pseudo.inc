;;; -*- mode: asm -*-


P_loadi:	MACRO reg, val
	lda #<val
	sta reg
	lda #>val
	sta reg+1
	ENDM

P_store:	MACRO src, dst
	ldy #0
	lda src
	sta (dst),y
	lda src+1
	iny
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
