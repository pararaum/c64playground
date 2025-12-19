.include "LAMAlib.inc"
.include "LAMAlib-sprites.inc"

DRIFT_UPDATE_INTERVAL=20
CREDITS_COLOR=7
SCREEN_BASE=$cc00
SPRBASE=$E000
N_SPRITES=8
SPR1X=(320-8*24)/2+24
SPR1Y=80
INCREASESTEP=(290-SPR1Y)/ITERATIONS
ITERATIONS=14
CHARSET_BASE=$D000
MUSIC_BASE=$1000
LOWEST_OVERHANG_SPRITE_POS=30

DX=5
DY=8

;-----------------------------------------------------
;-- parts when assembled standalone
;-- ass -d UNITTEST show.s

.ifdef UNITTEST

        poke MUSIC_BASE+3,$60
        set_raster_irq 0,isr0
        jmp over_it

;ass -d UNITTEST show.s

;charset
  .scope charset
CHARSET_BASE=$d000
EFFECT=0                  ;Effects: 1 italic
                        ;         2 italic, including lower case
                        ;         3 bold
                        ;         4 bold, including lower case
                        ;         5 lower half bold
                        ;         6 lower half bold, including lower case
                        ;         7 thin
                        ;         8 thin, including lower case
MEM_CONFIG=$32
.include "modules/m_copycharset.s"
  .endscope

over_it:
        m_init charset
        m_run charset
.endif ;.ifdef UNITTEST

;-- end of UNITTEST part -----------------------------
;-----------------------------------------------------


;-----------------------------------------------------
;-- init routines ------------------------------------
start:
        pokew $fffe,irq_entry   ;installl irq handler
        sei
        pokew $314,isr0
        cli

        lda #00
        sta $D01C ;Sprite multicolor mode register
        sta $D01D ;Sprite double width register.
        sta $D017 ;Sprite double height register.

	lda #$20
	for X,250,downto,1
	  sta SCREEN_BASE-1,x
	  sta SCREEN_BASE-1+250,x
	  sta SCREEN_BASE-1+500,x
	  sta SCREEN_BASE-1+750,x
	next

        set_VIC_addr SCREEN_BASE,CHARSET_BASE
        set_VIC_bank $c000
        poke 648,$cc

;display petscii
        ldax #petsciiimg0
        jsr displayPETSCII

;-- end of init routines -----------------------------
;-----------------------------------------------------

; place all sprites

;install_file "greetings/02.bin",SPRBASE


        ldax #cruncheddata
        stax _addr_of_crunched_data

        do
        ;show next credit
        ;ensure sprites are currently disabled
            do
                lda spr_movement_active
            loop while ne

        ;all credits done?
            dec credits_countdown
            if eq
                rts
            endif

        ;decrunch next spriteset
            lda 1
            pha
            poke 1,$35
            jsr _decrunch
            pla
            sta 1

        ;initialize sprite positioning and speed
            poke incstep,1
            poke sprite_next_costume_y,SPR1Y+INCREASESTEP
            poke costume_change_counter,ITERATIONS-1
            poke spr_y,SPR1Y

.repeat N_SPRITES,i
            setSpriteXY i,SPR1X+i*24,SPR1Y
            setSpriteCostume i,SPRBASE+i*64
            setSpriteColor i,CREDITS_COLOR
.endrep

        ;enable sprite movement
            poke spr_movement_active,1

        loop
exit:
        rts

dx_steps:
        .byte 3,3,4,4,5,5,6,6,6,7,7,7,8

dy_steps:
        .byte 7,8,9,10,11,11,12,12,13,13,14,14,15

masks:
.repeat 8,i
        .byte 1<<i
.endrep

;-----------------------------------------------------
;-- interrupt routines -------------------------------

irq_entry:
        pha
        txa
        pha
        tya
        pha
        jmp ($0314)

isr0:
        ;hide sprites with black color
        lda #0
.repeat N_SPRITES,i
        sta $d027+i
.endrep

        poke $D01B,$ff  ;sprites behind screen contents

        asl $d019
x_scroll=*+1
        lda #$c8
        sta $d016
y_scroll=*+1
        lda #$1b
        sta $d011

        do_every DRIFT_UPDATE_INTERVAL
scrollstate=*+1
            ldx #lissajous_entries-1
            lda lissajous_x,x
            ora #$c8
            sta x_scroll
            lda lissajous_y,x
            ora #$18
            sta y_scroll
            dec scrollstate
            if mi
                ldx #lissajous_entries-1
                stx scrollstate
            endif
        end_every

        pokew $314,isr1
        poke $d012,248  ;any value between 248 and 250 work here

        jsr MUSIC_BASE+3

spr_movement_active=*+1
        lda #0
        if eq
            jmp irq_exit
        endif

        ;wait until we pass a rasterline where we don't have sprites
        ;with a long music routine we might be already past that
        lda #LOWEST_OVERHANG_SPRITE_POS+22
:
        cmp $d012
        bcs :-

        ;recolor sprites
        lda #CREDITS_COLOR
.repeat N_SPRITES,i
        sta $d027+i
.endrep

        ;sprite movement
spr_y=*+1
        lda #80
.repeat N_SPRITES,i
        sta $d001+2*i
.endrep
        A_between 30,SPR1Y-1
        if cc
            poke spr_movement_active,0
            poke $d015,0
        else
            poke $d015,$ff
            lda spr_y
            clc
incstep=*+1
            adc #1
            sta spr_y
sprite_next_costume_y=*+1
            cmp #00
            if pl
costume_change_counter=*+1
                lda #00
                if ne
                ;next sprite costume set
                    do_every 3
                        jmp over_incstep
                    end_every
                    inc incstep
over_incstep:
                    lda sprite_next_costume_y
                    clc
                    adc #INCREASESTEP
                    sta sprite_next_costume_y
                    lda SCREEN_BASE+$3f8
                    clc
                    adc #N_SPRITES
                    tax
                    stx SCREEN_BASE+$3f8
.repeat N_SPRITES-1,i
                    inx
                    stx SCREEN_BASE+$3f8+1+i
.endrep
                    dec costume_change_counter
                endif
            endif
        endif
irq_exit:
        pla
        tay
        pla
        tax
        pla
        rti

.include "lissajous-tables.inc"

isr1:
        poke $d011,%00010011
        poke $d012,0
        poke $D01B,$0   ;sprites in front of screen contents
        asl $d019
        pokew $314,isr0
        jmp irq_exit

;-- end of interrupt routines ------------------------
;-----------------------------------------------------

sprite_next_costume_cntr: .byte 8
credits_countdown: .byte N_CREDITS+1

.include "exofdecrunch.s"
.include "jumper_in_space.asm"
.include "displayPETSCII.s"
cruncheddata:
.include "greetings/all_greetings_exo.inc"
