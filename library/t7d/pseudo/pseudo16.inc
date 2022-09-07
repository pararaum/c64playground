;;; -*- mode: asm -*-

	.IFNDEF	PSEUDO16_ALREADY_INCLUDED2022
	PSEUDO16_ALREADY_INCLUDED2022=1

;;; P_cmp src, dst; P_cmp #imm, dst
.MACRO	P_cmp	src, dst
	.scope
        .if (.match (.left (1, {src}), #))
        ; immediate mode
        lda	#>(.right (.tcount ({src})-1, {src}))
	cmp	1+(dst)
        bne	@end
        lda	#<(.right (.tcount ({src})-1, {src}))
	cmp	0+(dst)
        .else
        ; absolute or zero page
        lda	1+(src)
	cmp	1+(dst)
        bne	@end
	lda	0+(src)
        cmp	0+(dst)
        .endif
@end:
	.endscope
.ENDMACRO

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

;;; Load indirect from src into the register dst.
.MACRO	P_load	src, dst
	ldy #0
	lda (src),y
	sta dst
	iny
	lda (src),y	
	sta dst+1
.endmacro

;;; Load a byte pointer to by src into destination register. The high byte is set to zero.
;;; Modifies: A,Y
.MACRO	P_loadb	src, dst
	ldy	#0
	lda	(src),y
	sta	dst
	sty	dst+1
.endmacro

;;; Load an immediate value into a register.
;;; Modifies: A
.MACRO	P_loadi	val, reg
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

;;; Add two 16 bit numbers.
.MACRO	P_add	src, dst
	.if (.match (.left (1, {src}), #)) ; immediate?
	 .local	@IARG
	 @IARG=(.right (.tcount ({src})-1, {src}))
	 lda	dst
	 clc
	 adc	#<@IARG
	 sta	dst
	 lda	dst+1
	 adc	#>@IARG
	 sta	dst+1
	.else
	 clc
	 lda src
	 adc dst
	 sta dst
	 lda src+1
	 adc dst+1
	 sta dst+1
	.endif
.ENDMACRO

;;; Add 8-bit immediate to a 16-bit destination. Warning! Value is *not* sign extended!
.MACRO	P_addimm	val,dst
	.local	@skip
	clc
	lda	dst
	adc	#val
	sta	dst
	bcc	@skip
	inc	dst+1
	@skip:
.ENDMACRO

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

	.ENDIF
