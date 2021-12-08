; Swinging rasterbars with front-back effect
;
; The screen content during the rasterbars being in front is blanked out by switching the charset to an area with all 0
; Other than switching the screen pointer, switching the charset does take effect immediately
;
; Code: Wil
; Version 0.1

.include "LAMAlib.inc"

midpoint = (y_sineend-y_sine)/2
charmode_buffer = 2

        ;copy interrupt vectur to brk vector
        lda $314
        sta $316
        lda $315
        sta $317

        memset $3800,$3fff,0    ;ensure the alternative charset is empty

	textcolor 1
	clrscr
	newline
	newline	
	newline	
	primm "               vcc rules!"

        ldx #0
main:   
        sei
wait_upper_raster:
        bit $d011
        bmi wait_upper_raster

        ldy y_sine,x   ;the line where the next rasterline will start
        inx
	cpx #y_sineend-y_sine
        bcc noreset
        ldx #00
noreset:
	poke colors+1,7
	poke colorsend-1,7
	lda #%00010100  ;$400 Sceen, $1000 Charset
	cpx #midpoint
	bcc firsthalf
	poke colors+1,7
	poke colorsend-1,7
	lda #%00011110  ;$400 Screen, $3800 Charset
firsthalf:
	sta charmode_buffer

        store X
wait0:
        cpy $d012
        bne wait0

        ldx #colorsend-colors-1   
loop1:  lda colors,x    ;next color

	store X
	ldx 2
wait1:
        cpy $d012
        bcs wait1        ;wait until rasterline Y has passed

        sta $d020        
        sta $d021
	stx $d018	 ;possibly change charset
        iny              ;next rasterline = y+2
        iny
	restore X
        dex            
        bpl loop1
	poke $d018,%00010100

:  	bit $d011	;wait for somewhere down in the border
        bpl :-

        inc $d020
        brk            ;call interrupt
        nop
        dec $d020

        restore X
        jmp main


colors:
        .byte $00
        .byte $07,$07,$0a,$08,$02
        .byte $02,$08,$0a,$07,$07
colorsend:

y_sine:  .byte 87,87,87,87,85,85,83,83,81,81,79,77,75,73,71,69,69,67,65,63,61,59,57,55,55,53,53,51,51,49,49,49,49,49,49,49,51,51,53,53,55,55,57,59,61,63,65,67,67,69,71,73,75,77,79,81,81,83,83,85,85,87,87,87
y_sineend:

; Program to generate Y values:
; 
; 5 print"S"
; 10 fort=0to127
; 20 y=146+86*sin(t*6.283/128)
; 30 printc$;mid$(str$(int(y)),2);
; 40 c$=","
; 50 next


