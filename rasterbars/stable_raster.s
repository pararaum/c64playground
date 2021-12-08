; Stable raster interrupt, double IRQ method
; Code by Wil 2021 using LAMAlib macros
;
; thanks to the great tutorial and explanation at
; https://www.retro-programming.de/programming/nachschlagewerk/interrupts/der-rasterzeileninterrupt/raster-irq-endlich-stabil/


.include "LAMAlib.inc"

rasterline = 42	; rasterline defines where our sync process starts
; rasterline must not be values with a badline or a badline in the following 2 lines
; badlines are lines where the numer $0x07 equals the content of $D011 & 0x07
; standard setting of $D011=$1B, %00011011
; this lines between 50 and 249 with line mod 8 = 3, 4 or 5 won't work

        sei
	pokew $314,irq
        set_raster_irq rasterline
	sta $d019
        cli
	rts

irq:
	;we are somewhere in the middle of the rasterline 
	;38 cycles for the ROM IRQ routine + 0..7 jitter
	;next irq in current line+2 should hit it exactly

	lda #<irq2      ; 2 cycles
	sta $314	; 4
	lda #>irq2	; 2
	sta $315	; 4
	tsx		; 2
	store X		; 4
	lda #1		; 2 preload for acknowledging raster irq
	delay_cycles      6
			;--
			;26 cycles, we are now surely already in rasterline+1

	inc $d012	; 6 set next irq for rasterline+2
	sta $d019       ; 4 acknowledge IRQ
	cli		; 2

	;until now between 76 and 83 cycles passed since the first rasterline
	;the next one comes after 126 cycles or 83 cycles (if there was a badline)

	.repeat 26	;irq will happen for sure within these nops
	nop
	.endrep
        
irq2:
        ;38 or 39 cycles passed in this line
	restore X	; 2
	txs		; 2
	delay_cycles	  8
	lda #1		; 2 preload A
	bit $01         ; 3 just for 3 cycles
	ldx $d012	; 4
	cpx $d012	; 4
			;--
			;25 we are now already in next line or not 

.assert <* < 253, error, "Branch for synchronisation goes over memory page. Add 1-3 padding bytes before this line to fix it!"

	beq stable_raster   ;3 cycles if before, 2 otherwise

	;we are now definitely 2 cycles into rasterline
stable_raster:

        sta $d020                          ;Rahmenfarbe setzen
        delay_cycles     57
	lda #00		; 2
	sta $d020	; 4
	                ;--
			;63

	pokew $314,irq
        
        lda #<irq                        ;Original IRQ-Vektor setzen
        sta $0314
        lda #>irq
        sta $0315
rasterline_start=*+1        
        lda #<rasterline 
        sta $d012
        
        asl $d019
        
	lda 203
	switch A
	case 2	;crsr up/down
	  lda rasterline_start	
	  sec
	  sbc #1
	  bcs ok
          break
	case 7 ;crsr right/left
	  lda rasterline_start	
	  clc
	  adc #1
	  bcc ok
          break
	endswitch
	jmp $ea31
ok:
	sta rasterline_start
        jmp $ea31