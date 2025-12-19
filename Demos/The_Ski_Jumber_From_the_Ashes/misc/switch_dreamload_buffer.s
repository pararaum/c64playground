.include "dload.cfg"	;defines LdLoc

.export _switch_dreamload_buffer

; Switches the buffer address of the dream load routine
; to the memory page passed in A
; For example, if A=8, the new buffer adress is $800
;
; LdLoc must be set to the address of the loader code
.proc _switch_dreamload_buffer
        sta LdLoc+$28
        sta LdLoc+$3D
        sta LdLoc+$4E
        sta LdLoc+$5C
        sta LdLoc+$5F
        sta LdLoc+$74
        sta LdLoc+$77
        sta LdLoc+$86
        sta LdLoc+$8B
        sta LdLoc+$9D
        sta LdLoc+$B2
        sta LdLoc+$B5
        sta LdLoc+$180
        sta LdLoc+$18D
        sta LdLoc+$190
        rts
.endproc
