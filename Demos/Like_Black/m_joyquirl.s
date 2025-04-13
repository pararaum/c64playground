.include "LAMAlib.inc"

def_const JOY_CONTROL,2 ;1..joyport1, 2..joyport2
def_const COUNTERCLOCKWISE,0


run:    lda $dc02-JOY_CONTROL
        and #$0f
last_state=*+1
        ldx #$ff
        if mi
            ldx #joypatterns_end-joypatterns-1
:
            cmp joypatterns,x
            beq found_new_pattern
            dex
            bpl :-
found_new_pattern:
            stx last_state
            poke quirls,0
        else
            cmp joypatterns,x
            beq exit_rts          ;joystick did not move since last time
            store X
            dex
            if mi
                ldx #joypatterns_end-joypatterns-1
            endif
            cmp joypatterns,x
            beq exit_rts          ;joystick did move backwards, ignore
            restore X
            inx
            cpx #joypatterns_end-joypatterns
            if cs
                ldx #0
            endif
            cmp joypatterns,x
            if eq
                stx last_state
                inc quirls
            else
		poke last_state,$ff
                poke quirls,0
                sec
                rts
            endif
        endif
exit_rts:
        lda quirls
        clc
        rts

quirls:
        .byte 00

.if COUNTERCLOCKWISE
joypatterns:
;counterclockwise, including diagonal positions
        .byte %1110     ;up
        .byte %1010
        .byte %1011     ;left
        .byte %1001
        .byte %1101     ;down
        .byte %0101
        .byte %0111     ;right
        .byte %0110
joypatterns_end:
.else
joypatterns:
;clockwise, including diagonal positions
        .byte %1110     ;up
        .byte %0110
        .byte %0111     ;right
        .byte %0101
        .byte %1101     ;down
        .byte %1001
        .byte %1011     ;left
        .byte %1010
joypatterns_end:
.endif
