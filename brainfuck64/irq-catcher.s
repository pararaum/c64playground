;-----------------------------------------------------------------
; May-2020 V0.1
; Wilfried Elmenreich
; Code under CC-BY-4.0 license
;
; Routine to handle the IRQ if I/O and Kernal ROM are banked out
;
; calling it installs its irq vector in FFFE/FFFF
; as long as Kernal ROM is on, this routine is dormant
; if Kernal ROM is off, it catches the IRQ, turns on Kernal ROM 
; and calls the previously installed IRQ routine at $314/315
;-----------------------------------------------------------------

.export _irq_catcher_init

;value of memory 1 during interrupts

distinguishBRK = 0
mem1=$36	;configuration to be shown during IRQ

_irq_catcher_init:
	lda #<catchirq
	sta $fffe
	lda #>catchirq
	sta $ffff
	rts

catchirq: 
	;do what Kernal routine $FF48 would have done
	pha
	txa
	pha
	tya
	pha

	;push the data for a fake irq to the stack

	lda #>endirq
	pha
	lda #<endirq
	pha

	lda 1
	php	;dummy for status reg
	pha	;value of peek(1) will be in A
	pha	;just a placeholder for X value
	pha	;just a placeholder for Y value

	lda #mem1
	sta 1	;now the intended memory configuration is visible

.if distinguishBRK>0
	tsx
	lda $10a,x
	and #$10
	bne nmi_irq
.endif
	jmp ($314)	;this goes to $ea31 or a custom routine
.if distinguishBRK>0
nmi_irq:
	jmp ($316)
.endif	

endirq:
	sta 1	;recover original memory configuration state
	pla
	tay
	pla
	tax
	pla
	rti	;this ends the IRQ for good
