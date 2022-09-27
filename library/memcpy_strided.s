;;; Function to copy memory with strides.
	.include	"t7d/memoryfunctions.i"
	.importzp	ptr1,ptr2

	.bss
memcpy_strided_srcwidth:	.res	1
memcpy_strided_srcheight:	.res	1
memcpy_strided_srcstride:	.res	1
memcpy_strided_dststride:	.res	1

	.code
memcpy_strided:
	;; X=number of lines still to copy.
	ldx	memcpy_strided_srcheight
@l1:
	smallmemcpy_macro	ptr1, ptr2, memcpy_strided_srcwidth
	lda	memcpy_strided_srcstride
	clc
	adc	ptr1
	sta	ptr1
	bcc	@noov1
	inc	ptr1+1
@noov1:
	lda	memcpy_strided_dststride
	clc
	adc	ptr2
	sta	ptr2
	bcc	@noov2
	inc	ptr2+1
@noov2:
	dex
	bne	@l1
	rts
