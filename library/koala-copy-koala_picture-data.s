
	.include	"t7d/stackmacros.i"
	.include	"t7d/memoryfunctions.i"

	.importzp	ptr1, ptr2, ptr3

	.export	copy_koala_picture_data
	.export	copy_koala_picture_data_bg

	.code
;;; Copy Koala picture image-data to destination area.
;;; Input: A/X=image size, remaining pointers are expected behind the JSR.
;;; 	- source pointer (koala picture without load address)
;;; 	- destination bitmap data
;;; 	- destination chars (colour information)
;;; 	- destination colours (for colour RAM)
;;; Output: -
;;; Modifies: A, X, Y

;;; Input: A/X=adjust value, 16bit
;;; Output: ptr1, Y=2
	.proc	set_source_adjusted
	ldy	#1		; Get koala source pointer LO into ptr1
	clc			; Now add LO adjustment
	adc	(ptr3),y
	sta	ptr1		; Store in ptr1
	iny			; Index pointer increment to HI.
	txa			; Adjustment HI moved into A.
	adc	(ptr3),y	; Get HI value, Carry is taken care of!
	sta	ptr1+1
	rts
	.endproc

	.proc	setup_destination
	lda	(ptr3),y
	sta	ptr2
	iny
	lda	(ptr3),y
	sta	ptr2+1
	rts
	.endproc

.proc	main_copy_routine
	;; Bitmap
	lda	#0		; Get koala source pointer into ptr1
	tax
	jsr	set_source_adjusted
	ldy	#3		; Get destination bitmap into ptr2
	jsr	setup_destination
	lda	#<8000
	ldx	#>8000
	jsr	memcpy_up
	;; Chars
	lda	#<8000		; Get koala chars pointer into ptr1
	ldx	#>8000
	jsr	set_source_adjusted
	ldy	#5		; Get destination bitmap into ptr2
	jsr	setup_destination
	jsr	memcpy1000_via_ptr
	;; Cols
	lda	#<9000		; Get koala chars pointer into ptr1
	ldx	#>9000
	jsr	set_source_adjusted
	ldy	#7		; Get destination bitmap into ptr2
	jsr	setup_destination
	jmp	memcpy1000_via_ptr
.endproc

copy_koala_picture_data_bg:
	PullStoreStackptrLOCAL
	lda	@tempstackptrLO	; Get pointer to return addres -1 into ptr3
	sta	ptr3
	lda	@tempstackptrHI
	sta	ptr3+1
	jsr	main_copy_routine
	;; Background
	lda	#<10000		; Get koala BG pointer into ptr1
	ldx	#>10000
	jsr	set_source_adjusted
	ldy	#0
	lda	(ptr1),y
	sta	$d021
	RetrievePushStackptrAdjLOCAL 8
	rts

copy_koala_picture_data:
	PullStoreStackptrLOCAL
	lda	@tempstackptrLO	; Get pointer to return addres -1 into ptr3
	sta	ptr3
	lda	@tempstackptrHI
	sta	ptr3+1
	jsr	main_copy_routine
	RetrievePushStackptrAdjLOCAL 8
	rts

