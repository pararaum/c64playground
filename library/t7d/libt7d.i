; -*- mode: asm -*-

;;; Disable the CIA interrupts.
        .import _disable_cia_irq

;;; Set the kernal interrupt pointer.
;;; Input: addr=address to the kernal pointer to
;;; Modifies: A
;;; Output: -
.macro	SetIRQ314Pointer	addr
	lda	#<(addr)
	sta	$314
	lda	#>(addr)
	sta	$315
.endmacro

;;; Set the CPU's interrupt pointer.
;;; Input: addr=address to the kernal pointer to
;;; Modifies: A
;;; Output: -
.macro	SetIRQCPUPointer	addr
	lda	#<(addr)
	sta	$FFFE
	lda	#>(addr)
	sta	$FFFF
.endmacro

;;; Fill the colour RAM
;;; Input: A=colour value
;;; Output: -
;;; Modifies: X
	.import	_fill_colour_ram

;;; Fill exactly 1000 bytes with a value (e.g. screen).
;;; Input: A/X=pointer to memory, Y=value to fill with
;;; Output:
;;; Modifies: A,X,Y,ptr1
	.import	fill_1000_bytes

;;; Busy wait for a frame.
;;; This is achieved by polling $d011. The pm variant uses BPL/BMI the mp variant BMI/BPL.
;;; Input: -
;;; Output: -
;;; Modifies: -
	.import busywait_frame_pm
	.import busywait_frame_mp

;;; Busy wait several frames, uses busywait_frame_mp.
;;; Input: number=number of frames to wait
;;; Output: Y=0
;;; Modifies: (X),Y
	.macro BusywaitFramesMP number
	.if number > 255
	ldx	#>(number)
	.endif
	ldy	#<(number)
	jsr	busywait_frame_mp
	dey
	bne	*-4
	.if number > 255
	dex
	bne	*-7
	.endif
	.endmacro

;;; Default simple decrunch routine for pucrunch.
;;; Input: A/X=address of packed data
;;; Ouput: A/X=exec address
;;; Modifies: A, X, Y
	.import	pudecrunch_default
