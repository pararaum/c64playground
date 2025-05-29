;;; -*- mode: asm -*-
;;; Macros to ease stack manipulations.

;;; Counter to be used for local labels.

	.ifndef	LOCALLABELCOUNTER
	LOCALLABELCOUNTER .set	0
	.endif

;;; ------------------------------------------------------------------
;;; Macros to push/pop values (words) onto the stack.

;;; Macro to push a word, first LO then HI, onto the stack.
;;; Input: value, load value from this address
;;;	  #value, push immediate value
;;; Modifies: A
;;; Attention, this is the reverse as the CPU push a value onto the stack when executing JSR.
.macro	PushWordLH value
	;;  Value starts with '#'? → immediate value
        .if(.match(.left(1, {value}), #))
        lda	#<(.right(.tcount({value})-1, {value}))
	pha
        lda	#>(.right(.tcount({value})-1, {value}))
	pha
        .else
	lda	value
	pha
	lda	value+1
	pha
        .endif
.endmacro

;;; Macro to push a word, first LO then HI, onto the stack.
;;; Input: value, load value from this address
;;;	  #value, push immediate value
;;; Modifies: A
;;; Attention, this is the order as the CPU push a value onto the stack when executing JSR.
.macro	PushWordHL value
	;;  Value starts with '#'? → immediate value
        .if(.match(.left(1, {value}), #))
        lda	#>(.right(.tcount({value})-1, {value}))
	pha
        lda	#<(.right(.tcount({value})-1, {value}))
	pha
        .else
	lda	value+1
	pha
	lda	value
	pha
        .endif
.endmacro
.macro	PushWordJSR value
	PushWordHL value
.endmacro

;;; ------------------------------------------------------------------
;;; Macros to move memory to the stack and back. The CPU stack is used, so be careful as not much space is available.

;;; Push some memory to the stack
;;; Input: zpptr=memory pointer, size=number of bytes, init(optional)=initialise zpptr
;;; Modifies: A, Y
.macro PushMemoryToStack zpptr, size, init
	.local	@loop
	.ifnblank init
	 ldy	#<(init)
	 sty	zpptr
	 ldy	#>(init)
	 sty	zpptr+1
	.endif
	ldy	#0
@loop:	lda	(zpptr),y
	pha
	iny
	cpy	#(size)
	bne	@loop
.endmacro

;;; Pull some memory fropm the stack
;;; Input: zpptr=memory pointer, size=number of bytes, init(optional)=initialise zpptr
;;; Modifies: A, Y
.macro PullMemoryFromStack zpptr, size, init
	.local	@loop
	.ifnblank init
	 ldy	#<(init)
	 sty	zpptr
	 ldy	#>(init)
	 sty	zpptr+1
	.endif
	ldy	#(size)-1
@loop:	pla
	sta	(zpptr),y
	dey
	bpl	@loop
.endmacro

;;; ------------------------------------------------------------------
;;; The following is a pair of functions which are dangerous but can
;	help greatly in handling parameters via the stack. The come in
;	pairs:
;
;	* PullStoreStackptr
;	* RetrievePushStackptr

;;; Pull the return address of the stack and store it in a temporary area. Warning! If not used properly this will lead to hard to debug errors!
;;; Input: -
;;; Output: -
;;; Modifies: A
;;; Data: A unique label "UNIQUELABEL%04X"+offset is used to store the old stack pointer.
.macro	PullStoreStackptr
	pla
	sta	.ident(.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER))+4
	pla
	sta	.ident(.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER))+1
.endmacro

.macro	PullStoreStackptrLOCAL
	pla
	sta	@tempstackptrLO
	pla
	sta	@tempstackptrHI
.endmacro

;;; Retrieve the return address from the temporary area and push it back on the stack.
;;; Input: -
;;; Output: -
;;; Modifies: A
;;; After this macro the LOCALLABELCOUNTER is incremented!
.macro	RetrievePushStackptr
	.ident (.sprintf("UNIQUELABEL%04X", LOCALLABELCOUNTER)):
	lda	#0
	pha
	lda	#0
	pha
	LOCALLABELCOUNTER .set LOCALLABELCOUNTER+1
.endmacro
.macro	RetrievePushStackptrLOCAL
	lda	#0
	@tempstackptrHI=*-1
	pha
	lda	#0
	@tempstackptrLO=*-1
	pha
.endmacro

;;; Macro to put back the adjusted return address onto the stack. Use in conjunction with PullStoreStackptrLOCAL
;;; Input: delta=bytes to add.
;;; Output: Return address+delta on stack.
;;; Modifies: A, X
.macro	RetrievePushStackptrAdjLOCAL delta
	lda	#0		; Overwritten LO value.
	@tempstackptrLO=*-1
	clc			; Adjust now.
	adc	#delta
	tax			; But keep safe in X.
	lda	#0		; Overwritten HI value.
	@tempstackptrHI=*-1
	adc	#0		; Take care of carry.
	pha
	txa			; Now get LO.
	pha			; And put on stack.
.endmacro


;;; Adjust the stack pointer.
;;; Input: -
;;; Output: A=new stack pointer value
;;; Modifies: A, X

.macro AdjustStackptr delta
	tsx			; Get current stack pointer.
	txa			; Move stack pointer to A.
	clc
	adc	#<(delta)	; Add value in a to stack pointer. For the use of <(…), see https://github.com/cc65/cc65/issues/186 or https://sourceforge.net/p/cc65/mailman/message/32234634/.
	tax			; X=A as the stack pointer can be transfered to X only.
	txs			; Change the stack pointer to the new value.
.endmacro

;;; Adjust the stack pointer via A.
;;; Input: A=delta to add onto the stack pointer
;;; Output: A=new stack pointer value
;;; Modifies: A, X
	.macro AdjustStackptrA
	.local	@tmp
	sta	@tmp
	tsx			; Get current stack pointer.
	txa			; Move stack pointer to A.
	clc
	adc	#$00		; value in a to stack pointer (via self-modifying code).
	@tmp = *-1
	tax			; X=A as the stack pointer can be transfered to X only.
	txs			; Change the stack pointer to the new value.
	.endmacro

;;; Remove (!) the return address minus one (!) and put it into a pointer (destptr).
;;; Warning, remember to use one-based access!
;;; see L.A. Leventhal, W. Saville, "6502 Assembly Language Subroutines", Osborne/McGraw-Hill, 1982, p. 44.
;;; Input: (Stack)
;;; Output: -
;;; Modifies: A
	.macro	GetReturnAddrIntoPointer destptr
	PLA			; LSB
	STA	0+(destptr)
	PLA			; MSB
	STA	1+(destptr)
	.endmacro

	.macro	PointerToStack	ptr
	lda	1+(ptr)
	pha
	lda	0+(ptr)
	pha
	.endmacro

;;; If using the above macro to get the return address minus one into a pointer then this macro will put the return address back on the stack. It can be adjusted so that following parameters will be skipped.
;;; Can be used to implement parameter passing via program memory, see L.A. Leventhal, W. Saville, "6502 Assembly Language Subroutines", Osborne/McGraw-Hill, p. 44.
;;; Input: retaddr
;;; Output: (Stack)
;;; Modifies: A,Y
	.macro	PointerAdjustedToStack retaddr, adjust
	lda	0+(retaddr)
	clc			; Clear even if adjust == 0!
	.if (adjust) <> 0
	adc	#(adjust)
	.endif
	tay			; Keep LSB safe.
	lda	#0		; Prepare A, C unchanged
	;; Now add value in memory with carry, result in A!
	adc	1+(retaddr)
	pha			; MSB
	tya
	pha			; LSB
	.endmacro

;;; If using the GetReturnAddrIntoPointer macro to get the return address minus one into a pointer then this macro will put the return address back on the stack. It can be adjusted by a value in A so that following parameters will be skipped.
;;; Input: A, retaddr
;;; Output: (Stack)
;;; Modifies: A,Y
	.macro	PointerAAdjustedToStack retaddr
	clc
	adc	0+(retaddr)
	tay			; Keep LSB safe.
	lda	#0		; Prepare A, C unchanged
	;; Now add carry to value in memory, result in A!
	adc	1+(retaddr)
	pha			; MSB
	tya
	pha			; LSB
	.endmacro
