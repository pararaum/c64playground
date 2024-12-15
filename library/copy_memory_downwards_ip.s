	.include	"t7d/stackmacros.i"
	.include	"t7d/pseudo/pseudo16.inc"
	.importzp	ptr1, ptr2, ptr3, tmp1
	.import	memcpy_down

	.export	copy_memory_downwards_ip

copy_memory_downwards_ip:
	GetReturnAddrIntoPointer	ptr3
	ldy	#1
	lda	(ptr3),y	; Get destination address into ptr2.
	sta	ptr2
	iny
	lda	(ptr3),y
	sta	ptr2+1
	iny			; Get size into A,X
	lda	(ptr3),y
	sta	tmp1		; Keep safe in tmp1.
	iny
	lda	(ptr3),y
	tax			; Put size HI into X.
	P_transfer	ptr3, ptr1 ; Copy return address to ptr1
	P_addimm	5,ptr1	; Adjust ptr1 to point to source area.
	P_addimm	4,ptr3	; Skip the data after the JSR call.
	lda	tmp1		; Put size LO int A.
	clc			; Now skip the data after the call.
	adc	ptr3		; LO
	tay			; Put new LO of return address into Y for later use.
	txa			; Get size HI.
	adc	ptr3+1
	pha			; Push HI on stack.
	tya			; LO of return address into A.
	pha			; And push it.
	lda	tmp1		; Load size LO int A, again.
	jmp	memcpy_down
