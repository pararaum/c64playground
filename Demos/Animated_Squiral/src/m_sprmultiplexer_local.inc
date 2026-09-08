;***********************************************************************
;* LOCAL COPY for Animated_Squiral demo (Kernal-off environment).
;* Difference to upstream m_sprmultiplexer.s:
;*   Adds def_const IRQ_EXIT_ADDR / IRQ_EXIT_ADDR_DONE so the three
;*   hardcoded jmp $ea81 / jmp $ea31 can be redirected to a local
;*   PLA/TAY/PLA/TAX/PLA/RTI stub when the Kernal ROM is swapped out.
;*   Defaults preserve the upstream behaviour.
;***********************************************************************
;*
;* Module: Sprite Multiplexer with Overlay Support
;* Version 2.2 (local patch)
;* by Wil
;* Date: 2026-02-20
;*
;* Purpose:
;* Raster-IRQ sprite multiplexer supporting 24+ logical sprites
;* using only 8 hardware sprites. Includes Y-priority depth sorting,
;* optional two-layer overlay sprites for extra colors, and a grounded
;* sprite mode for platformers.
;*
;* Sprite visibility convention: msprite_y = 0 means hidden.
;* Use showSprite/hideSprite macros to show/hide sprites safely.
;*
;* Features:
;* - Configurable number of multiplexed sprites
;* - feature for Y-priority depth sorting
;* - Overlay support: two hardware sprites per logical sprite for extra colors
;* - Grounded sprite: one sprite always rendered behind overlapping others
;* - updateSpriteAttributes: reads color/overlay from sprite data automatically
;* - PRE_ROUTINE hook: called at raster start (before sort), ideal for music
;* - POST_ROUTINE hook: called after all sprites drawn, ideal for game logic
;* - Zero page or normal RAM for sprite Y array (configurable)
;* - Debug mode: border color shows raster budget consumption
;*
;* Configuration and Inclusion:
;*
;* .include "LAMAlib.inc"
;*
;* jmp over_modules
;*
;* .scope sprmux
;*   MAXSPRITES=14
;*   ENABLE_YPRIORITY=1
;*   ENABLE_GROUNDED=1
;*   PRE_ROUTINE=music_player
;*   .include "modules/m_sprmultiplexer.s"
;* .endscope
;*
;* over_modules:
;*   m_init sprmux
;*
;* Main Program Usage:
;*
;* Include LAMAlib-muplex-sprites.inc for the sprite macros.
;* Macros are compatible with LAMAlib-sprites.inc naming.
;*
;* setSpriteX 0, 100
;* setSpriteY 0, 100
;* setSpriteCostume 0, 128
;* setSpriteColor 0, 2
;* showSprite 0
;*
;***********************************************************************

.include "LAMAlib.inc"
.include "LAMAlib-modulemacros.inc"

.macpack longbranch

;***********************************************************************
;* parameters - can be overwritten from main

def_const MAXSPRITES,16			; maximum number depends on enabled features, CPU load, and available zero page memory)

def_const ENABLE_OVERLAY,1		; automatically puts a second sprite on top when overlay flag is set for this sprite
def_const ENABLE_YPRIORITY,1            ; prioritizes sprites with a higher Y coordinate in front.
def_const ENABLE_GROUNDED,1             ; allows the selection of a single sprite to be rendered behind others (like an object on the ground). Only to be used with YPRIORITY
def_const ENABLE_UPDATE_ATTRIBUTES,1	; adds a routine to automatically detect color, multicolor mode and overlay from the 64th byte of the sprite data
def_const SPRITES_UNDER_ROM,0           ; set to 1 to support reading sprite data beneath ROM

def_const PRE_ROUTINE,0			; set an address of a routine to be called before the multiplexer starts. Routine must end with an RTS
def_const ENABLE_PRE_ROUTINE,0		; set to 1 when PRE_ROUTINE is used (separate from address so .if works with label addresses)
def_const POST_ROUTINE,0		; set an address of a routine to be called after the multiplexing is done. Routine must end with a JMP $EA81 or similar
def_const ENABLE_POST_ROUTINE,0		; set to 1 when POST_ROUTINE is used (separate from address so .if works with label addresses)

def_const IRQ_EXIT_ADDR,$ea81		; jmp target for mid-frame IRQ shortcut exits. Override when Kernal is off.
def_const IRQ_EXIT_ADDR_DONE,$ea31	; jmp target for end-of-frame exit (used only when POST_ROUTINE=0). Override when Kernal is off.

def_const SPRMUX_NO_ZP,0		; set to 1 to avoid usage of zero page memory. Makes the code less performant.

def_const RASTERLINE_START,262		; rasterline at which the multiplexer should start its preparations
def_const DEBUG_RASTER_TIME,0		; enable to show fram border color changes when the multiplexer is active

;***********************************************************************
;* Parameter validation

.if ENABLE_GROUNDED
  .if .not ENABLE_YPRIORITY
    .error "ENABLE_GROUNDED requires ENABLE_YPRIORITY=1 (grounded needs depth sorting)"
  .endif
.endif

;***********************************************************************
;* Data structures

.if SPRMUX_NO_ZP=0
.zeropage
msprite_y: .res MAXSPRITES
.exportzp _msprite_y_ov := msprite_y
.else
.code
msprite_y: .res MAXSPRITES
.export _msprite_y_ov := msprite_y
.endif

.code

sprite_index: .res MAXSPRITES
msprite_x: .res MAXSPRITES
msprite_x_hi: .res MAXSPRITES
msprite_col: .res MAXSPRITES
msprite_costume: .res MAXSPRITES
msprite_assignedx2: .res MAXSPRITES
msprite_y_shadow: .res MAXSPRITES

.if ENABLE_OVERLAY
msprite_col_overlay: .res MAXSPRITES
.export _msprite_col_overlay_ov := msprite_col_overlay
.endif

; Note: msprite_grounded is defined inline as self-modifying code target
; See: msprite_grounded=*+1 in the grounded sprite handling code
.if ENABLE_GROUNDED
.export _msprite_grounded_ov := msprite_grounded
.endif

; Configuration constant exports
.exportzp _mmaxsprites := MAXSPRITES

; Standard exports
.export _msprite_x_ov := msprite_x
.export _msprite_x_hi_ov := msprite_x_hi
.export _msprite_y_shadow := msprite_y_shadow
.export _msprite_costume_ov := msprite_costume
.export _msprite_col_ov := msprite_col
.export _sprite_index_ov := sprite_index

;***********************************************************************
;* Exported routine addresses for optional features

.if ENABLE_UPDATE_ATTRIBUTES
.export _sprmultiplexer_UpdateSpriteAttributes := update_sprite_attributes
.endif

;***********************************************************************
;* init target

init:
        sei
        poke $d015,$ff          ; Enable all hardware sprites
        
        ; Initialize sprite_index array
        for X,MAXSPRITES-1,downto,0
            txa
            sta sprite_index,x
        next
        
        set_raster_irq RASTERLINE_START
        pokew $314,multiplexer_isr0
        
        ; Patch screen base addresses in code
        lda PTRSCRHI
        ora #3
.if ENABLE_OVERLAY
        sta screenbase_plus3_1
        sta screenbase_plus3_2
.endif
        sta screenbase_plus3_3
        
        ; Note: msprite_grounded is initialized inline as #$ff
        
        cli
        rts

;***********************************************************************
;* disable_multiplexer method

disable_multiplexer:
        sei
        poke $d015,$00
        pokew $314,minimum_isr
        cli
        rts

minimum_isr:
.if ENABLE_PRE_ROUTINE
        jsr PRE_ROUTINE
.endif
        asl $d019

.if ENABLE_POST_ROUTINE
	jmp POST_ROUTINE
.else
	jmp IRQ_EXIT_ADDR_DONE
.endif

;***********************************************************************
;* Multiplexer IRQ handler

multiplexer_isr0:

.if DEBUG_RASTER_TIME
        ldx $d020
        stx recover_border_color
        inc $d020
.endif

.if ENABLE_PRE_ROUTINE
        jsr PRE_ROUTINE
.endif

.if DEBUG_RASTER_TIME
        inc $d020
.endif
        jsr sort_sprites

.if DEBUG_RASTER_TIME
        inc $d020
.endif

        lda $d011
        and #$7f
        sta $D011       ; Clear MSB in VIC's raster register

        lda #0
.repeat 8,i
        sta $d001+2*i  ; Clear sprite Y positions
.endrep

        ldx #MAXSPRITES-1
skip_sprites_with_y_coord_0:
        ldy sprite_index,x
        lda msprite_y,y
        bne found_sprite_to_display
        dex
        bpl skip_sprites_with_y_coord_0
        jmp multiplexing_done

found_sprite_to_display:
        stx nextspr_idx
        pokew $314,multiplexer_nextspr_isr

.if ENABLE_YPRIORITY
        ; =====================================================================
        ; Complex assignment with restart logic (ENABLE_YPRIORITY=1)
        ; Original algorithm from sprmultiplexer_overlay.s
        ; =====================================================================
        
        stx startindex
        poke firstrestart,1
        
        lda #0
        for y,21-12,downto,0    ; 21 = sprite height, 12 = min raster lines needed to restart at hw spr 7
            sta restart_index,y
        next
        
assign_loop_restart:
        ; Set new y-limit for next restart
        ldy sprite_index,x
        lda msprite_y,y
        adc #24                 ; 24 = min y distance between two sprites sharing hw spr 7 on restart
        sta y_limit
        
        poke curr_hwspr_num, 7*2        ; HW sprite numbers are multiplied by 2
        poke lastsaferestart,0
        
assign_next_hw_sprite:
        ldy sprite_index,x      ; Get index of current sprite
        
.if ENABLE_OVERLAY
        ; Is it an overlay sprite? Then it needs two hw sprite positions
        lda msprite_col_overlay,y
        cmp #1  ; Set carry if A!=0
.else
        clc     ; No overlay support, always single sprite
.endif
        
        ; Assign the current HW sprite to the current multiplexed sprite
curr_hwspr_num=*+1
        lda #$AF
        sta msprite_assignedx2,y
        
.if ENABLE_OVERLAY
        if cs           ; Carry is still defined from previous cmp #1
            sbc #2      ; Decrease A (value of curr_hwspr_num) by 2 to get next HW sprite idx
        endif
.endif
        sta curr_hwspr_num
        
        ; Sign flag from curr_hwspr_num
        if pl   ; Was the current sprite assigned to proper (positive) HW sprite?
            ; Check if the gap to the next multiplexed sprite is larger than a sprite height
            
            ; Y contains index of current sprite
            lda msprite_y,y
            sta sprite_y
            ldy sprite_index-1,x  ; Get index of next sprite
            
            ; Reverse subtraction, calculate the value of arg - A
            clc
            sbc msprite_y,y       ; Subtract A from the y coordinate of next sprite
            eor #$ff
            
            cmp #21
            if cs
                ; Found out that we could safely restart at sprite 7 here
                stx lastsaferestart
            else
                tay
                stx unsaferestart
sprite_y=*+1
                lda #$AF
y_limit=*+1
                cmp #$AF    ; Only store the restart point if we have a safe distance from hw sprite 7
                if cs
                    cpy #12     ; 12 = min raster gap needed to safely restart at hw spr 7
                    if cs
add_heuristic_restart:
                        txa
                        sta restart_index-12,y  ; 12 = min raster gap
                    else
                        store A
                        ; Compare x values to see if the next two sprites overlap
                        ldy sprite_index-1,x
                        lda msprite_x,y
                        ldy sprite_index,x
                        sec
                        sbc msprite_x,y
                        clc
                        adc #24-1
                        cmp #25
                        if cc
                            restore A
                            clc
                            adc #3      ; heuristic bonus: horizontally overlapping sprites get
                                        ; 3 extra raster lines credited, reducing unnecessary restarts
                            cpy #12     ; 12 = min raster gap
                            bcs add_heuristic_restart
                        endif
                    endif
                endif
            endif
            cpx #$01      ; Signed comparison for X <= 0 using negative flag
            bmi assignment_done
        endif ; if pl
        
        dec curr_hwspr_num    ; Update value for next hardware sprite number
        dec curr_hwspr_num
        bmi restart_with_hwsprite7      ; We ran out of hardware sprites, need to restart the count
        dex
        jmp assign_next_hw_sprite

restart_with_hwsprite7:
        ldy #21-12+1            ; 21 = sprite height, 12 = min raster gap
find_best_restart:
        ldx restart_index,y
        bne found_restart
        dey
        bpl find_best_restart
found_restart:
        ; Reset remaining entries
        lda #0
reset_restarts:
        sta restart_index,y
        dey
        bpl reset_restarts
        ; X contains the restart index
        dex     ; X contains restart index, we continue after the safe position
        
        ; Check if this was the first restart, if yes shift the sprites to ease the multiplexer's job
firstrestart=*+1
        lda #$AF
        beq to_assign_loop_restart
        
        ; We are shifting the first batch of sprites towards lower indices, easing the multiplexer's work
        ldy sprite_index+1,x
.if ENABLE_OVERLAY
        lda msprite_col_overlay,y
        cmp #1  ; Set carry if A!=0
.else
        clc
.endif
        lda msprite_assignedx2,y
.if ENABLE_OVERLAY
        if cs   ; Triggers if msprite_col_overlay,y was !=0
            sec
            sbc #2
        endif
.endif
        sta backshift
        
        store X                 ; Save the index for later

adjust_first_wave_of_multiplexed_sprites:
        inx                     ; The first time, this inx also regains the index of the last placed sprite
        ldy sprite_index,x
        lda msprite_assignedx2,y
        sec
backshift=*+1
        sbc #$AF
        sta msprite_assignedx2,y
startindex=*+1
        cpx #$AF
        bne adjust_first_wave_of_multiplexed_sprites
        
        dec firstrestart        ; firstrestart = 0
        
        restore X
to_assign_loop_restart:
        jmp assign_loop_restart

unsaferestart=*
restart_index: .res 21-12+2    ; 21 = sprite height, 12 = min raster gap
lastsaferestart=*-1

; These variables are stored inline using self-modifying code:
; startindex, backshift, curr_hwspr_num, lastsaferestart
; unsaferestart, firstrestart, y_limit, sprite_y

assignment_done:

.if ENABLE_GROUNDED
        ; Takes care that sprite with number msprite_grounded will be drawn behind all other overlapping sprites
msprite_grounded=*+1
        lda #$ff
        if pl   ; If negative, there is no grounded sprite
            ; Find idx of laying sprite
            ldy nextspr_idx
            .byte $a2       ; Opcode for ldx # to skip over first DEY and ensure BEQ is not taken
search_loop:
            dey
            beq done_grounding
            cmp sprite_index,y
            bne search_loop
            ; Now adjust all sprites above within range
shift_ground_spr:
            tax
            lda msprite_y,x
            sec
            sbc #21
            sta y_limitg
            
            lda msprite_assignedx2,x
            cmp #$0e
            beq done_grounding
            sta g_assigned
            stx g_spr_idx
shift_down:
            iny     ; Next sprite
            ldx sprite_index,y
            lda msprite_y,x
y_limitg=*+1
            cmp #$af
            bcc done_grounding
            lda msprite_assignedx2,x
g_assigned=*+1
            cmp #$af
            bcc done_grounding
            dec msprite_assignedx2,x
            dec msprite_assignedx2,x
g_spr_idx=*+1
            ldx #$af
            sta msprite_assignedx2,x
            jmp shift_down
        endif

done_grounding:
.endif   ; ENABLE_GROUNDED

.else ; ENABLE_YPRIORITY
        ; =====================================================================
        ; Simple assignment (ENABLE_YPRIORITY=0)
        ; No restart logic - just assign sprites to HW sprites 0-7 in order
        ; =====================================================================
        
;todo lda #LAST_HW_SPR*2
        lda #7*2                ; start with HW sprite 7 (*2)
        sta curr_hw_spr_simple

	inx	;compensate for dex in first loop iteration
assign_simple_loop:
        ; Move to next multiplexed sprite
        dex
        bmi assign_simple_done	; check if we've gone through all sprites (wrapped to 255)
        
	ldy sprite_index,x      ; get sprite number from sorted index
        
        ; Check if visible (y != 0)
;notneeded        lda msprite_y,y
;notneeded        beq skip_invisible_simple
        
        ; Assign current HW sprite to this multiplexed sprite
curr_hw_spr_simple=*+1
        lda #0
        sta msprite_assignedx2,y
;todo add overlay handling

        ; Move to next HW sprite (decrement by 2)
	if eq
;todo lda #LAST_HW_SPR*2
          lda #7*2 	
	else
	  sec
          sbc #2	   
	endif
        sta curr_hw_spr_simple
        

        jmp assign_simple_loop
assign_simple_done:

.endif  ; ENABLE_YPRIORITY

.if DEBUG_RASTER_TIME
        inc $d020
.endif

        ; Wait for new screen to start
waitloop:
        bit $d011       ; Busy wait in case the new screen has not started yet
        bmi waitloop    ; Because multiplexer needs single byte y coordinates

.if DEBUG_RASTER_TIME
recover_border_color=*+1
        ldx #00
        stx $d020
.endif
        ; Fallthrough to multiplexer_nextspr!

;***********************************************************************
;* multiplexer_nextspr - Display next sprite

multiplexer_nextspr_isr:
.if DEBUG_RASTER_TIME
	inc $d020
.endif

multiplexer_nextspr: ;entry point for isr and loop for multiplexed sprites

nextspr_idx=*+1
        ldx #00
        ldy sprite_index,x

nextspr_idx_set:
        ldx msprite_assignedx2,y
.if ENABLE_OVERLAY
        lda msprite_col_overlay,y
        if ne                   ;it is an overlay sprite
            dex                   ;we need to check the availability of the next hw sprite
            dex
        endif
.endif

;***********************************************************************
; is sprite ready to be re-placed?
; if not, calculate planned_rasterline
; if planned_rasterline <3 away, calc max(planned_rasterline,next_sprite_y-15)
; if still too near, busy wait, otherwise set IRQ and give back control to main program

        lda $d001,x
        beq dospr               ;if y coordinate is 0, the sprite is free to use
        cmp #233 ;TODO: MAX_SPR_Y               ;everything after this coordinate is irrelevant
        bcs sprites_done_trampolin

        clc
        adc #$16                ;$16 = 22 -> height of a sprite + 1
        sta $d012               ;proactively set next rasterline for irq
                                ;(storing to $D012 is setting raster line for next interrupt)
        sbc $d012               ;is the previous sprite already drawn?
                                ;(loading from $D012 is current rasterline)
        bcc dospr
        cmp #3
        if cs
            asl $d019             ;acknowledge raster interrupt
.if DEBUG_RASTER_TIME
	    dec $d020
.endif
            jmp IRQ_EXIT_ADDR
        endif
        ;less than three rasterlines until planned IRQ
        lda $d001,x
        ;clc                    ;we know carry is clear here because before we exit if carry is set
        adc #$16
        sta cmpmax
        sta ldmax
                                ;can we go for a longer stint?
        lda msprite_y,y
        ;clc                    ;we know carry is clear here
        sbc #9                  ;we subtract one more
cmpmax=*+1
        cmp #$af
        if cc                   ;next m_sprite_y -9 < y of current sprite to replace + 22
ldmax=*+1
            lda #$af              ;we are already in a hurry, load current sprite to replace + 22
        endif
        store A
        sta $d012               ;proactively set next rasterline for irq
        sec
        sbc $d012               ;is the targeted rasterline already reached?
        cmp #3
        if pl
            asl $d019             ;acknowledge raster interrupt
.if DEBUG_RASTER_TIME
	    dec $d020
.endif
            jmp IRQ_EXIT_ADDR
        endif
        restore A
busywait_raster:
        cmp $d012
        bcs busywait_raster
        bcc dospr               ;unconditional jump

skip_spr:
        dec nextspr_idx
        bpl multiplexer_nextspr ;unconditional jump

; trampolines for long branch
sprites_done_trampolin:
        jmp sprites_done

;***********************************************************************
;* do next sprite

.import _twopotentials,_maskedtwopotentials

dospr:
        lda msprite_y,y
        cmp $d012
        bcc skip_spr            ;darn! we missed the rasterline to draw this sprite

.if ENABLE_OVERLAY
        lda msprite_col_overlay,y
        if ne
          ;--------------------------------------
          ;- Setting an overlay sprite

          ;X contains now the current hw sprite number*2 for the overlay sprite
            lda msprite_y,y
            sta $d001+2,x         ;set y coordinate of underlying sprite
            sta $d001,x           ;set y coordinate of overlay sprite
            lda msprite_x,y
            sta $d000+2,x         ;set x coordinate of underlying sprite
            sta $d000,x           ;set x coordinate of overlay sprite
            txa                   ;next registers need index 0..7, so half it
            lsr                   ;since A contains an even number here, C=0 afterwards
            tax
          ;X contains now the current hw sprite number for the overlay sprite
            lda msprite_col,y
            sta $d027+1,x         ;set sprite color

            if pl                 ;is it a single color sprite?
                lda $d01c
                and _maskedtwopotentials+1,x   ;disable multicolor for beneath sprite
                and _maskedtwopotentials,x     ;disable multicolor for overlay sprite
            else                  ;the else part is 2 cycles faster, therefore chose the more likely action here
                lda $d01c
                ora _twopotentials+1,x         ;enable multicolor for beneath sprite
                and _maskedtwopotentials,x     ;disable multicolor for overlay sprite, overlay is always single color
            endif
            sta $d01c             ;set sprite multicolor
            lda msprite_col_overlay,y
            sta $d027,x           ;set sprite color of overlay sprite
            lda msprite_costume,y
screenbase_plus3_1=*+2
            sta $400+$3f8+1,x     ;set sprite costume of underlying sprite
          ;clc                  ;we know that C=0 because of previous LSR
            adc #01
screenbase_plus3_2=*+2
            sta $400+$3f8,x       ;set sprite costume of overlay sprite

            lda msprite_x_hi,y
            if ne
                lda $d010
                ora _twopotentials+1,x      ;enable 8th bit of X coordinate for underlying sprite
                ora _twopotentials,x        ;enable 8th bit of X coordinate for overlay sprite
            else                  ;the else part is 2 cycles faster, therefore chose the more likely action here
                lda $d010
                and _maskedtwopotentials+1,x ;disable 8th bit of X coordinate for underlying sprite
                and _maskedtwopotentials,x   ;disable 8th bit of X coordinate for overlay sprite
            endif
            sta $d010             ;set sprite X msb
          ;dex                  ;decrement X necause we used 2 sprites
            jmp after_sprite_setting
          ;- End of setting an overlay sprite
          ;--------------------------------------
        endif
.endif

        ;--------------------------------------
        ;- Setting a single (non-overlay) sprite

        ;X contains now the current hw sprite number*2
        lda msprite_y,y
        sta $d001,x             ;set y coordinate
        lda msprite_x,y
        sta $d000,x             ;set x coordinate (bits 0-7)

        txa                     ;next registers need index 0..7
        lsr
        tax

        lda msprite_col,y
        sta $d027,x             ;set sprite color
        if pl
            lda $d01c
            and _maskedtwopotentials,x
        else                    ;the else part is 2 cycles faster, therefore chose the more likely action here
            lda $d01c
            ora _twopotentials,x
        endif
        sta $d01c               ;set sprite multicolor

        lda msprite_costume,y
screenbase_plus3_3=*+2
        sta $400+$3f8,x ;set sprite costume

        lda msprite_x_hi,y
        if ne
            lda $d010
            ora _twopotentials,x
        else                    ;the else part is 2 cycles faster, therefore chose the more likely action here
            lda $d010
            and _maskedtwopotentials,x
        endif
        sta $d010               ;set sprite X msb
        ;- End of setting a single sprite
        ;--------------------------------------

after_sprite_setting:
        dec nextspr_idx
        bmi sprites_done
        jmp multiplexer_nextspr

sprites_done:
.if RASTERLINE_START>255
        lda $d011
        ora #$80
        sta $D011       ;set most significant bit in VIC's raster register
.endif
        poke $d012,<RASTERLINE_START    ;next IRQ at the original first rasterline

        lda #<multiplexer_isr0
        sta $314
        lda #>multiplexer_isr0
        sta $315

multiplexing_done:
        asl $d019       ;acknowlegde raster interrupt

.if DEBUG_RASTER_TIME
	dec $d020
.endif

.if ENABLE_POST_ROUTINE
	jmp POST_ROUTINE
.else
	jmp IRQ_EXIT_ADDR_DONE
.endif


;***********************************************************************
; sort sprites according to their y coordinate

sort_sprites:

; unroll inner bubble loop
; the unroll is coded here as a two by two sequence
; roles of y and x change every second block, this saves a load instruction when going forward

SORT_DOWNWARDS = 1       ;we want the sprite with the largest y coordinate first

.repeat MAXSPRITES/2, sprnum
.ident(.sprintf("back%d", 2*sprnum)):
        ldx sprite_index + 2*sprnum
.ident(.sprintf("over%d", 2*sprnum)):
        ldy sprite_index + 2*sprnum+1
  .if SORT_DOWNWARDS=0
        lda msprite_y,y
        cmp msprite_y,x
  .else
        lda msprite_y,x
        cmp msprite_y,y
  .endif
        bcs .ident(.sprintf("over%d", 2*sprnum+1))
        stx sprite_index + 2*sprnum+1
        sty sprite_index + 2*sprnum

  .if sprnum>0
        bcc .ident(.sprintf("back%d", 2*sprnum-1))
  .endif

  .if sprnum<MAXSPRITES/2-1

.ident(.sprintf("back%d", 1+2*sprnum)):
        ldy sprite_index + 1+2*sprnum
.ident(.sprintf("over%d", 2*sprnum+1)):
        ldx sprite_index + 1+2*sprnum+1
    .if SORT_DOWNWARDS=0
        lda msprite_y,x
        cmp msprite_y,y
    .else
        lda msprite_y,y
        cmp msprite_y,x
    .endif
        bcs .ident(.sprintf("over%d", 1+2*sprnum+1))
        sty sprite_index + 1+2*sprnum+1
        stx sprite_index + 1+2*sprnum

        bcc .ident(.sprintf("back%d", 1+2*sprnum-1))

  .else

.ident(.sprintf("over%d", 2*sprnum+1)):

  .endif

.endrep
        rts

;***********************************************************************
;* update_sprite_attributes
;* Called via updateSpriteAttributes macro - sprite number in A on entry
;* Reads sprite attribute byte from sprite data, sets color and
;* (if ENABLE_OVERLAY) overlay color for the multiplexed sprite.
;* No zero page used - self-modifying code for address operands.
;* SPRITES_UNDER_ROM: if 1, temporarily switches to all-RAM to read
;* sprite data that lives beneath BASIC/KERNAL ROM.

.if ENABLE_UPDATE_ATTRIBUTES

update_sprite_attributes:
        ldx #0
        stx spraddr             ; clear self-mod address low byte
        tax                     ; sprite number A -> X for indexed ops

        lda 648                 ; screen base (=$d018 content * 64 + VIC bank base)
        and #%11000000          ; mask VIC bank bits
        sta addbank             ; patch into ora instruction below

        lda _msprite_costume_ov,x   ; get logical sprite costume pointer
        lsr
        ror spraddr             ; shift bit 0 into address low byte
        lsr
        ror spraddr             ; shift bit 1 into address low byte
addbank=*+1
        ora #00                 ; fold in VIC bank -> high byte
        sta spraddr+1           ; patch self-mod address high byte

.if ENABLE_OVERLAY
        sta spraddr2+1          ; patch second self-mod address (overlay read)
        lda spraddr
        sta spraddr2            ; patch second self-mod address low byte
.endif

        ldy #$3f                ; offset to attribute byte within sprite block

.if SPRITES_UNDER_ROM
        lda 1                   ; save CPU I/O port
        pha
        lda #$34                ; all-RAM: BASIC+KERNAL off, I/O off
        sei
        sta 1
.endif

spraddr=*+1
        lda $affe,y             ; get sprite attribute byte (self-mod absolute,y)
        sta _msprite_col_ov,x   ; store as sprite color

.if ENABLE_OVERLAY
        and #%00010000          ; bit 4: overlay-next-sprite flag
        if ne
          ldy #$7f              ; offset to next sprite's attribute byte
spraddr2=*+1
          ora $affe,y           ; combine flag with next sprite attribute (self-mod)
        endif
        sta _msprite_col_overlay_ov,x
.endif

.if SPRITES_UNDER_ROM
        pla
        sta 1                   ; restore CPU I/O port
        cli
.endif

        rts

.endif ; ENABLE_UPDATE_ATTRIBUTES
