.include "LAMAlib.inc"

; petscii2x.py -d -f ASM vcc01.petscii.c vcc11.petscii.c vcc21.petscii.c vcc31.petscii.c -o vcc-petsciis.s

        jmp over_it

.include "displayPETSCII.s"

.scope colorfade
FADE_DELAY=1
.include "m_colorfade.s"
.endscope

over_it:
        poke pic_num,0
        do
pic_num=*+1
            lda #00
            asl
            tay
            lda petsciidir,y
            ldx petsciidir+1,y

            jsr displayPETSCII
            delay_ms 200
        ;getkey
            ldx pic_num
            inx
            cpx petsciinum
            if cs
                break
            else
                stx pic_num
            endif
        loop
	delay_ms 1000
colorfade_loop:
        m_run colorfade
        delay_ms 100
        dec counter
        bne colorfade_loop
        rts
counter: .byte 10

.include "vcc-petsciis.s"
