;;; -*- mode: asm -*-

.MACRO	P_and	src, dst
	lda src
	and dst
	sta dst
	lda src+1
	and dst+1
	sta dst+1
.endmacro

.MACRO	P_eor	src, dst
	lda src
	eor dst
	sta dst
	lda src+1
	eor dst+1
	sta dst+1
.endmacro

.MACRO	P_load	src, dst
	ldy #0
	lda (src),y
	sta dst
	iny
	lda (src),y	
	sta dst+1
.endmacro

;;; Load a byte pointer to by src into destination register. The high byte is set to zero.
.MACRO	P_loadb	src, dst
	ldy	#0
	lda	(src),y
	sta	dst
	sty	dst+1
.endmacro
	
.MACRO	P_loadi	reg, val
	lda #<(val)
	sta reg
	lda #>(val)
	sta reg+1
.endmacro

.MACRO	P_pull	reg
	pla
	sta reg
	pla
	sta reg+1
.endmacro

.MACRO	P_push	reg
	lda reg+1
	pha
	lda reg
	pha
.endmacro

;;; Push a 16-bit value immediate on the stack, can be used for jump tables
.MACRO	P_pushi	data
	lda #>(data)
	pha
	lda #<(data)
	pha
.endmacro

.MACRO	P_shiftl	reg
	asl reg
	rol reg+1
.endmacro

.MACRO	P_shiftr	reg
	lsr reg+1
	ror reg
.endmacro

.MACRO	P_store	src, dst
	ldy #0
	lda src
	sta (dst),y
	lda src+1
	iny
	sta (dst),y	
.endmacro

.MACRO	P_storeb	src, dst
	ldy #0
	lda src
	sta (dst),y
.endmacro

.MACRO	P_sub	src, dst
	sec
	lda dst
	sbc src
	sta dst
	lda dst+1
	sbc src+1
	sta dst+1
.endmacro

.MACRO	P_transfer	src, dst
	lda src
	sta dst
	lda src+1
	sta dst+1
.endmacro

.MACRO	P_add	src, dst
	clc
	lda src
	adc dst
	sta dst
	lda src+1
	adc dst+1
	sta dst+1
.endmacro

.MACRO	P_branchNZ	reg, disp
	lda reg+1
	bne disp
	lda reg
	bne disp
.endmacro

.MACRO	P_branchZ	reg, disp
	lda reg+1
	bne @out
	lda reg
	beq disp
@out:
.endmacro

.MACRO	P_exit
	nop
.endmacro

.macro	P_inc	dst
	.local	@skip
	inc	dst
	bne	@skip
	inc	dst+1
@skip:
.endmacro

.macro	P_dec	dst
	.local	@skip
	lda	dst
	bne	@skip
	dec	dst+1
@skip:	dec	dst
.endmacro
