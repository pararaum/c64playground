;;; Ripped from http://www.antimon.org/dl/c64/code/missing.txt.

; Missing Cycles by Pasi 'Albert' Ojala   (po87553@cs.tut.fi albert@cc.tut.fi)
;                          
;                           Missing Cycles
;                           --------------
;        [all timings are in PAL, the principle applies to NTSC too]
; 
; Everybody knows that there are 63 cycles available to the C64 processor on
; each scan line, except for one which only provides 23 cycles (later referred
; to as a "bad" scan line). But what happens when we add sprites and why ?
; 
; In the C64, the VIC (video interface controller) has much more to do than
; just showing graphics on the screen. It also handles the memory refresh.
; On each scanline, it has to refresh five rows in the memory matrix and
; fetch fourty bytes of graphics data.
; 
; The VIC does all of this during the cycles (phase 1) that the processor is
; not using the memory.  These cycles, however, are not sufficient when the
; VIC also needs to access the character and color codes for the next row.
; The memory bus can't be used by the CPU and the VIC at the same time, so CPU
; access to the bus must be denied to allow the VIC to fetch its data.
; Fortunately, the VIC bus (12-bit wide) allows the character (8 bits) and
; color (4 bits) codes to be fetched at the same time.
; 
; 
; _Understanding how sprites work_
; 
; If there are sprites on the screen, the VIC needs even more cycles to fetch
; all of the graphics data. Scan lines are time divided so that there is
; enough time for all action during one line. On each line, the sprite
; image pointers are fetched during phase 1. If the sprite is to be displayed
; on that line, the three bytes of image data are fetched right after that.
; Out of these three fetches, two take place during phase 2 of the clock,
; so the processor will lose these. On average, two clock cycles are lost
; for each sprite that is displayed on that line.
; 
; But how is it possible for all eight sprites to only take 16-19 cycles
; (depending on the timing) when we have observed that one sprite requires
; three cycles? And why do sprites 0, 2, 4, 6 and 7 together take up as many
; cycles as all eight sprites ? The answer may be found in the way the VIC
; tells the CPU that it needs additional cycles.
; 
; 
; _The BA signal_
; 
; When the VIC wants to use the bus, the BA (Bus Available) signal goes
; inactive. This will happen three cycles before the bus must be released !
; During these three cycles, the CPU must complete all memory accesses or
; delay them until it has the bus again.
; 
; The CPU either completes the current instruction in the remaining cycles
; or sits and waits for the bus to become available again. It can't execute
; a new instruction as long as it doesn't have the bus. This is why cycles
; seem to be lost (besides those stolen directly for the sprites). Usually,
; all 8 sprites take 17 cycles while one sprite takes three cycles. However,
; the CPU may continue to execute an instruction if it does not use the bus.
; 
; 
; _Theory and speculation_
; 
; Let's suppose that all the sprites are enabled and on the same scan line.
; Then, the VIC steals 16 cycles (2 cycles for each sprite) for the memory
; fetches and 3 cycles as overhead for the BA signal, for a total of 19 cycles.
; However, it will be usually less because the CPU will use some of the cycles
; when the bus request is pending.
; 
; If we now disable sprite 4, no cycles are released for the CPU's use. This
; is because during the previous sprite 4 data fetch, the VIC already signals
; that it needs the bus for the sprite 5 data fetch and BA stays low (Refer
; to the timing chart). Thus, the CPU never sees BA go high during sprite 4
; and 2 cycles are still lost.
; 
; Accordingly, if we only turn off sprites 1, 3 and 5 we get no cycles back
; from the VIC. So in time-critical raster routines, always use sprites in
; order.
; 
; 
; _What can we do with this feature ?_
; 
; How can this be useful? A good use is for synchronization. Normally,
; before the CPU starts to execute the raster interrupt code, it's executing
; an instruction of undefined cycle-length. This execution time varies from
; two to seven cycles.
; 
; With a sprite, you can do the synchronization with a minimal effort using
; a DEC or INC instruction in the right place. If the processor is early,
; it has to wait for the bus, otherwise it will continue to execute cycles
; from the instruction.
; 
; I have never experimented with any other instruction than DEC/INC, but
; some others should work also. You need an instruction which has a cycle that
; do not need the bus to be available. e.g. INC $3fff will increase the
; value during the fifth cycle and do not need the bus for that.
; 
; 
; _A demo program_
; 
; The enclosed program includes a short raster color routine to demonstrate
; this strict timing and synchronization. The background color is changed
; 12 times on each line. The electron beam runs over eight pixels during
; one cycle, so the timing must be precise.
; 
; --------------------------------------------------------------------------
; _Table for PAL VIC timing for the Missing cycles_
; 
; 
; 012345678901234567890123456789012345678901234567890123456789012 cycles
; 
; Normal scan line, 0 sprites
; ggggggggggggggggggggggggggggggggggggggggrrrrr  p p p p p p p p  phi-1 VIC
;                                                                 phi-2 VIC
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx phi-2 6510
; 63 cycles available
; 
; Normal scan line, 8 sprites
; ggggggggggggggggggggggggggggggggggggggggrrrrr  pspspspspspspsps phi-1 VIC
;                                                ssssssssssssssss phi-2 VIC
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXXX                 phi-2 6510
; 46-49 cycles available
; 
; Normal scan line, 4 sprites
; ggggggggggggggggggggggggggggggggggggggggrrrrr  psp psp psp psp  phi-1 VIC
;                                                ss  ss  ss  ss   phi-2 VIC
; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXXX              xx phi-2 6510
; 48-51 cycles available
; 
; Bad scan line, 0 sprites
; ggggggggggggggggggggggggggggggggggggggggrrrrr  p p p p p p p p  phi-1 VIC
; cccccccccccccccccccccccccccccccccccccccc                        phi-2 VIC
;                                         xxxxxxxxxxxxxxxxxxxxxxx phi-2 6510
; 23 cycles available
; 
; Bad scan line, 8 sprites
; ggggggggggggggggggggggggggggggggggggggggrrrrr  pspspspspspspsps phi-1 VIC
; cccccccccccccccccccccccccccccccccccccccc       ssssssssssssssss phi-2 VIC
;                                         xxxxXXX                 phi-2 6510
; 4-7 cycles available
; 
; 
; g= grafix data fetch (character images or graphics data)
; r= refresh
; p= sprite image pointer fetch
; c= character and color CODE fetch during a bad scan line
; s= sprite data fetch
; x= processor executing instructions
; X= processor executing an instruction, bus request pending
; 
; Observe! The left edge of the chart is not the left edge of the screen nor
; 	 the left edge of the beam, but the sprite x-coordinate 0. If you
; 	 have opened the borders, you know what I mean. A sprite can be
; 	 moved left from the coordinate 0 by using x-values greater than 500.
;  ___________
; |  _______  |<-- Maximum sized video screen
; |||       | |
; |||       |<-- Normal C64 screen
; |||       | |
; |||_______| |
; ||          |
; ||__________|
;  ^ Sprite coordinate 0
; 
; 
; --------------------------------------------------------------------------
; Demonstration program for missing cycles


COLOR0= $CE00  ; Place for color bar 0
COLOR1= $CF00  ; Place for color bar 1
RASTER= $FA    ; Line for the raster interrupt
DUMMY= $CFFF   ; Timing variable

	*= $C000-2
	.word $C000
        SEI             ; Disable interrupts
        LDA #$7F        ; Disable timer interrupts
        STA $DC0D
        LDA #$01        ; Enable raster interrupts
        STA $D01A
        STA $D015       ; Enable Sprite 0
        LDA #<IRQ       ; Init interrupt vector
        STA $0314
        LDA #>IRQ
        STA $0315
        LDA #$1B
        STA $D011
        LDA #RASTER     ; Set interrupt position (inc. 9th bit)
        STA $D012
        LDA #RASTER-20  ; Sprite will just reach the interrupt position
        STA $D001       ;  when it is positioned 20 lines earlier

        LDX #51
        LDY #0
        STA $D017       ; No Y-enlargement
LOOP0   LDA COL,X       ; Create color bars
        PHA
        AND #15
        STA COLOR0,X
        STA COLOR0+52,Y
        STA COLOR0+104,X
        STA COLOR0+156,Y
        PLA
        LSR
        LSR
        LSR
        LSR
        STA COLOR1,X
        STA COLOR1+52,Y
        STA COLOR1+104,X
        STA COLOR1+156,Y
        INY
        DEX
        BPL LOOP0
        CLI             ; Enable interrupts
        RTS             ; Return


IRQ     NOP             ; Wait a bit
        NOP
        NOP
        NOP
        LDY #103        ; 104 lines of colors (some of them not visible)
			; Reduce for NTSC, 55 ?
        INC DUMMY       ; Handles the synchronization with the help of the
        DEC DUMMY       ;  sprite and the 6-clock instructions
			; Add a NOP for NTSC

FIRST   LDX COLOR0,Y    ; Do the color effects
SECOND  LDA COLOR1,Y
        STA $D020
        STX $D020
        STA $D020
        STX $D020
        STA $D020
        STX $D020
        STA $D020
        STX $D020
        STA $D020
        STX $D020
        STA $D020
        STX $D020
			; Add a NOP for NTSC (one line = 65 cycles)
        LDA #0          ; Throw away 2 cycles (total loop = 63 cycles)
        DEY
        BPL FIRST       ; Loop for 104 lines

        STA $D020
        LDA #103        ; For subtraction
        DEC FIRST+1     ; Move the bars
        BPL OVER
        STA FIRST+1
OVER    SEC
        SBC FIRST+1
        STA SECOND+1

        LDA #1          ; Ack the raster interrupt
        STA $D019
        JMP $EA31       ; Jump to the standard irq handler

COL     .BYTE $09,$90,$09,$9B,$00,$99,$2B,$08,$90,$29,$8B,$08,$9C,$20,$89,$AB
        .BYTE $08,$9C,$2F,$80,$A9,$FB,$08,$9C,$2F,$87,$A0,$F9,$7B,$18,$0C,$6F
        .BYTE $07,$61,$40,$09,$6B,$48,$EC,$0F,$67,$41,$E1,$30,$09,$6B,$48,$EC
        .BYTE $3F,$77,$11,$11
                        ; Two color bars
