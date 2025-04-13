	.include	"consts.inc"

	.export	fldirq

	.data
cosvpos:
        ;;      .byt    7, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7
        ;;      .byt 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 3
        .byt    30, 33, 36, 39, 42, 45, 47, 50, 52, 54, 55, 57, 58, 59, 59, 60, 59, 59, 58, 57, 55, 54, 52, 50, 47, 45, 42, 39, 36, 33, 30, 26, 23, 20, 17, 14, 12, 9, 7, 5, 4, 2, 1, 0, 0, 0, 0, 0, 1, 2, 4, 5, 7, 9, 12, 14, 17, 20, 23, 26
cosvposend:
vpos:	.byte	0

	.bss
old11:	.res	1
old16:	.res	1

	.code
.proc	fldirq
fld:    lda	$d011
	sta	old11
	beq	nofld
	lda	$d016
	sta	old16
	lda	#0
	sta	$d016
	ldy vpos
        ldx cosvpos,y
	iny
        cpy #cosvposend-cosvpos
        bne l106
        ldy #00
l106:   sty vpos
	cpx	#0 ; Zero FLD lines?
	beq	nofld
	lda	$d012
syncwait:
	cmp	$d012
	beq	syncwait
l129:	lda $d012
        cmp $d012
        beq *-3
        clc
        adc #3
        and #7
        ora #$78
	dec	$d021
	.repeat	18
	nop
	.endrepeat
	inc	$d021
        sta $d011
        dex
        bne l129
	lda	old16
	sta	$d016
nofld:	lda	old11
	ora	#$80
	sta	$d011
	rts
.endproc
