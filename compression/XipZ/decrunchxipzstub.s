
DECRUNCHTARGET = $F7
DEFAULTDESTINATIONADDR = $33c
NUMBEROFBITS = 5
ENDADDRESS = $1000

;;; This variable should contain a single bit moving from left to right so that after eight LSR instructions it becomes zero and can be restored via ROR. Neat trick for counting eight bits.
bitcount = $2b			; $01 initially (basic text)

	.include	"basic.i"
	.export	decrunch
	.export	tocopy
	.export	tocopy_end

	.segment	"EXEHDR"
	.word	thebrk
	.word	770
	.byte	$9e,"2061"
thebrk:	brk
	brk
	brk

	.data
tocopy:
	;; 	.byte	"copy"
tocopy_end:

	.rodata
parameters:
	.word	ENDADDRESS
	.word	tocopy_end
	.byte	"t7d"
	.word	tocopy
parameters_end:

;;; Must be entered with X=0, Y=0, A=0.
;;; Must be entered via decrunch_entry!!!
decrunchdata:
	.org	DECRUNCHTARGET
	.proc	decrunch
first:	tay			; Compare A to zero for test if literal or compressed.
	beq	ldyn		; zero = compressed
	ldy	#8		; Literal, therefore get eight bits.
	.byte	$2c		; Skip the following instruction.
ldyn:	ldy	#NUMBEROFBITS	; ldy #n, load the number of bits for a compressed token.
decrunch_entry:
getbit:
	lsr	bitcount	; Update the bit counter (move the single bit).
	bne	skip		; Less than eight bits moved.
	ror	bitcount	; Put the bit in the carry into the MSB (reset counter).
	inx			; Increment source pointer.
	bne	skip		; Overflow?
	inc	sourceptr+1	; Increment high byte.
	;; Check for end.
	pha
	lda	sourceptr+1
	cmp	#>ENDADDRESS
	bne	@nojmp
	jmp	64738		; If not overwritten, reset the machine.
@nojmp:	pla
skip:
	asl	a:0,x		; Get next bit from source.
	sourceptr = *-2
	rol			; Move the bit into the accumulator.
	dey			; Decrement the bit counter.
	bmi	first		; Was this the first bit? It will decide if literal or compressed.
	bne	getbit		; All bits moved into A?
found:	bcs	lit		; bit 9 of .A
	tay			; Moved index in compression table.
	lda	transtab,y	; Get the value.
lit:	sta	DEFAULTDESTINATIONADDR ; Now save the byte.
	dest = *-2
	ldy	#0		; Y must be zero when we reenter the loop. It was set if got the byte via the table.
	tya			; Clear the accumulator so that no left over bits muddy the result.
	inc	dest		; Increment destination pointer.
	bne	getbit		; Overflow?
	inc	dest+1		; Increment high.
	bne	getbit		; Get next bit (unconditional jump).
transtab:
	;; Be careful as only 127 bytes are copied, if code + table is larger this will cause havoc!
;	.byte	"trns"
;	.res	64-4
	.endproc
	.reloc
decrunchdata_end:

	.code
_main:				; Must be the first code so that SYS works.
	;; Copy the copy parameters.
	ldx	#parameters_end-parameters-1
@pl:
	lda	parameters,x	; Get the three parameters.
	sta	z:$58,x		; Store them in the ZP.
	dex			; Next one.
	bpl	@pl		; parameter loop
	jsr	MEMORY_MOVE	; Leaves with X=0
	;; Now $58/$59 points to beginning of data-256!
	;; X=0, Y=0
	sei
@cplp:	lda	decrunchdata,y
	sta	a:DECRUNCHTARGET,y
	iny
	bne	@cplp
	;; Y=0 here!
	lda	$58		; Now add $00ff (256-1) to the pointer.
	clc
	adc	#$ff
	sta	decrunch::sourceptr
	lda	$59
	adc	#$00		; Adjust to beginning of compressed data.
	sta	decrunch::sourceptr+1
	txa
	jmp	decrunch::decrunch_entry
