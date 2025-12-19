;-----------------------------------------------
; PETSCII Decode and display
; to use, load address of compressed PETSCII img in A/X
; and call displayPETSCII:
;  lda #<petsciiimg5
;  ldx #>petsciiimg5
;  jsr displayPETSCII
;
; Version 3.00 April 2025
; Last changed: 2025-04-05
;-----------------------------------------------

.include "LAMAlib.inc"

def_const DECODE_FROM_D000,0	  ;if 1 the compressed PETSCII can lie anywhere in RAM,including I/O area $D000-$DFFF
                                  ;note that with that option your IRQ must be turned off or be able to handle an all RAM configuration
def_const ENABLE_TRANSPARENT,1	  ;if 1 a selectable character (default 0) will be treated as being transparent
def_const TRANSPARENT_CHARACTER,0 ;index of the character treated as transparent
def_const COMPACT_ZEROPAGE, 0     ;if 1 the module operates in a compact mode using only 2 zeropage addresses, resulting in 3% performance decrease
def_const TARGET_COLORMAP,0	  ;if 0 the value of $d800 is used as default
def_const TARGET_SCREEN,0	  ;if 0 the value in 648 is used as the high byte default value

.export displayPETSCII

zpptr=_llzp_word1

.zeropage
zp_srcptr: 	.res 2
.if COMPACT_ZEROPAGE=0
const_E0:	.res 1
const_10:	.res 1
.endif
.code

.macro disable_transparent
        lda #$c9        ;opcode CMP to overwrite BEQ
        sta ::_transparent_petscii_char +2
        sta ::_transparent_petscii_char2+2
.endmacro

.macro set_transparent screencode
        lda #screencode
        sta ::_transparent_petscii_char +1
        sta ::_transparent_petscii_char2+1
        lda #$f0        ;opcode BEQ
        sta ::_transparent_petscii_char +2
        sta ::_transparent_petscii_char2+2
.endmacro

displayPETSCII:
.ifndef DECODE_FROM_D000
DECODE_FROM_D000=0
.endif

decode_routine:
        sta zp_srcptr
        stx zp_srcptr+1
.if DECODE_FROM_D000=1
        lda $1
        pha
        poke 1,$34      ;all RAM configuration
.endif
.if TARGET_SCREEN=0
        lda PTRSCRHI
.else
	lda #>TARGET_SCREEN
.endif	
        sta scr+2
        sta scr2+2

.if TARGET_COLORMAP=0
        lda #$d8
.else
	lda #>TARGET_COLORMAP
.endif	
        sta colr+2
        sta colr2+2
        lda #0
        sta scr+1
        sta scr2+1
        sta colr+1
        sta colr2+1


        ldy #00
        lda (zp_srcptr),y
.if DECODE_FROM_D000=1
        inc $1  ;enable I/O
.endif
        sta $D020
.if DECODE_FROM_D000=1
        dec $1  ;all RAM configuration
.endif
        iny
        lda (zp_srcptr),y
.if DECODE_FROM_D000=1
        inc $1  ;enable I/O
.endif
        sta $D021
.if DECODE_FROM_D000=1
        dec $1  ;all RAM configuration
.endif
.if COMPACT_ZEROPAGE
const_E0=*+1
.endif
        and #$E0
        ;sta mrk+1
        sta mrk2+1
.if COMPACT_ZEROPAGE=0
        lda #$E0
        sta const_E0
        lda #$10
        sta const_10
.endif
        ldx #00

loop1:
bytes_to_repeat=*+1
	lda #0
	if ne
	  dec bytes_to_repeat
	  if eq
	    pla
	    tay
	    pla
	    sta zp_srcptr+1
          endif
	endif

        iny
        bne skphi
        inc zp_srcptr+1
skphi:  lda (zp_srcptr),y

        ;is it a special char?
;mrk:    eor #$E0 ;operation already done by converter
        bit const_E0
        beq special
mrk2:   eor #$E0

.if ENABLE_TRANSPARENT
        ;skip writing if transparent char
_transparent_petscii_char:
        cmp #00
        beq skip_transparent
.endif
scr:    sta $400,x
col:    lda #00
.if DECODE_FROM_D000=1
        inc $1  ;enable I/O
.endif
colr:   sta $d800,x
.if DECODE_FROM_D000=1
        dec $1  ;all RAM configuration
.endif
skip_transparent:
        inx
        bne loop1
	jsr updatetargetptrs
        jmp loop1

special:
        bit const_10
        beq repcode
        ;color code
        sta col+1
        sta col2+1
        jmp loop1

repcode:
        and #$0f
        beq repeat_n_offset
	cmp #$0e
	bcs special_command
repeat_next_byte_A_times:
        sta rep+1
        iny
        bne skphi2
        inc zp_srcptr+1
skphi2: lda (zp_srcptr),y
        sta loop2+1
        sty rcvy+1      ;save y for later

rep:    ldy #00
.if DECODE_FROM_D000=1
        inc $1  ;enable I/O
.endif
loop2:  lda #00

.if ENABLE_TRANSPARENT
_transparent_petscii_char2:
        cmp #00
        beq skip_transparent2
.endif

scr2:   sta $400,x
col2:   lda #00
colr2:  sta $d800,x
skip_transparent2:
        inx
	if eq
	  jsr updatetargetptrs
	endif
endofloop:
        dey
        bne loop2
.if DECODE_FROM_D000=1
        dec $1  ;all RAM configuration
.endif
rcvy:   ldy #00
        jmp loop1

.if DECODE_FROM_D000=1
_exit_rts:
        pla
        sta $1
        rts
.else
_exit_rts:
        rts
.endif

special_command:
	; 0 ... repeat n,-x
	; 15 .. end of pic
	; 14 .. long repeat

	cmp #$0f
	beq _exit_rts
	; fall through

long_repeat:
	iny
        bne skphi3
        inc zp_srcptr+1
skphi3:  lda (zp_srcptr),y
	jmp repeat_next_byte_A_times

repeat_n_offset:
	iny
	if eq
	  inc zp_srcptr+1
	endif	
	lda (zp_srcptr),y	;read number of bytes to repeat
	sta bytes_to_repeat

	iny
	if eq
	  inc zp_srcptr+1
	endif	
	lda zp_srcptr+1
	pha		;push current high-byte
	tya
	pha		;push current pointer
	clc
	adc (zp_srcptr),y	;change Y by offset
	if cc
	  dec zp_srcptr+1
	endif
	tay		
	jmp skphi	;entry to loop1 without increasing y

.if COMPACT_ZEROPAGE
const_10:	.byte $10
.endif

updatetargetptrs:
	inc scr +2
	inc scr2+2
	inc colr +2
	inc colr2+2
	rts
