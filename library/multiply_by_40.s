;;; Routine to multiply by 40 for getting the offset to the line. Uses the global free pointer ptr1 which can be changed by any function.

	.importzp	ptr1

	.export	y_times_40

.proc y_times_40
        lda     #0
        sta     ptr1+1           ; Clear high of pointer
        tya                     ; A = y-pos
        asl                     ; Multiply by 8
        rol     ptr1+1
        asl
        rol     ptr1+1
        asl
        rol     ptr1+1
        sta     ptr1             ; Low byte of pointer.
	lda	ptr1+1
        pha
	lda	ptr1
	pha
        asl     ptr1             ;*16
        rol     ptr1+1
        asl     ptr1             ;*32
        rol     ptr1+1
        pla                     ; y-pos*8, LO
        clc
        adc     ptr1
        sta     ptr1
	pla			; y-pos*8, HI
        adc	ptr1+1
	sta	ptr1+1
        rts
.endproc
