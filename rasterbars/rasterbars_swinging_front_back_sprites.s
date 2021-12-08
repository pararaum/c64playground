; Swinging rasterbars with front-back effect
; Code: Wil
; Version 0.1
;
; The screen content during the rasterbars being in front is covered by 7 X-expanded sprites
; Due due the fact that sprites eat part of the cycles in the border, this apporach has timing problems
;
; What I measured (PAL timing)
;
; Normal timing of a rasterline (no badline)
; CCCCCCCCCCCCccccKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKccccCCC
;
; C = CPU cycle in the invisble border
; c = CPU cycle in the visible border
; K = CPU cycle in the screen area
;
;
; Timing with 7 sprites enabled and on the same line as the current rasterline (8 sprites would make no difference):
; ssssssssCCCCccccKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKXXXssssssss
;
; s = cycles used by VIC for reading sprite data
; X = CPU has a pending bus request, but can finish a store
;
;
; The left edge of the charts is the beginning of the rasterline (number in $d012 changes)

.include "LAMAlib.inc"

sprnum=13

        ;copy interrupt vectur to brk vector
        lda $314
        sta $316
        lda $315
        sta $317

	;set up sprite data

	ldx #00
	ldy #0
:	lda spritelines,y
	sta sprnum*$40,x
	inx
	sta sprnum*$40,x
	inx
	sta sprnum*$40,x
	inx
	sta sprnum*$40,x
	inx
	sta sprnum*$40,x
	inx
	sta sprnum*$40,x
	iny
	inx
	cpx #63
	bcc :-

	lda #sprnum
	ldx #7
:	lda #sprnum
	sta $400+$3f8,x	;initialize sprite costumes
	lda #8
	sta $d027,x	;initialize sprite colors
	dex
	bpl :-

	poke $d026,$a	;set sprcolor
	
	.repeat 7,I
	poke $D000+I+I,<(24+I*48)	;set sprite X coordinates
	.endrep
	poke $D010,$60	;two sprites are beyond coordinate 255

	lda #$7f
	sta $D01D	;expand
	sta $D015	;enable
	sta $D01C	;mulicolor sprites

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
        store X

	.repeat 7,I
	sty $D001+I+I	;set sprite Y coordinates
	.endrep

	lda colorsend-1,x
	sta $d025

wait0:
        cpy $d012
        bne wait0

        ldx #colorsend-colors-1   
loop1:  lda colors,x    ;next color
wait1:
        cpy $d012
        bcs wait1        ;wait until rasterline Y has passed

;	.repeat 24	;burn some cycles
;	nop
;	.endrep 

        sta $d020        
        
	lda sprcolors,x
	sta $d025

        iny              ;next rasterline = y+2
        iny
        dex            
        bpl loop1

        ldy #10        ;wait for somewhere down in the border
wait2:  cpy $d012
        bcc wait2

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

spritelines:
        .byte $55,$55,$ff,$aa,$55
        .byte $55,$aa,$ff,$55,$55

	.byte 0,0	;buffer bytes for shorter sprites

sprcolors:
	.byte 7,7,7,7,2,2,2,2,2,7,7,7

y_sine:  .byte 69,69,71,73,75,77,79,81,81,83,83,85,85,87,87,87,87,87,87,87,85,85,83,83,81,81,79,77,75,73,71,69,69,67,65,63,61,59,57,55,55,53,53,51,51,49,49,49,49,49,49,49,51,51,53,53,55,55,57,59,61,63,65,67,67,69,71,73,75,77,79,81,81,83,83,85,85,87,87,87,87,87,87,87,85,85,83,83,81,81,79,77,75,73,71,69,69,67,65,63,61,59,57,55,55,53,53,51,51,49,49,49,49,49,49,49,51,51,53,53,55,55,57,59,61,63,65,67
y_sineend:

; Program to generate Y values:
; 
; 5 print"S"
; 10 fort=0to127
; 20 y=146+86*sin(t*6.283/128)
; 30 printc$;mid$(str$(int(y)),2);
; 40 c$=","
; 50 next


