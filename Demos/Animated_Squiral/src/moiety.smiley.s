;;; =====================================================================
;;; moiety.smiley.s
;;; ---------------------------------------------------------------------
;;; Multiplexed smiley blob for Animated_Squiral demo.
;;;
;;; Strategy: full take-over during the smiley phase.
;;;   smiley_init is called ONCE from the frameengine (channel 0). It
;;;   sets up the sprite multiplexer, redirects $FFFE to a local IRQ
;;;   shim, enables IRQs, busy-waits while the multiplexer drives the
;;;   display for Smiley_runtime frames, then tears everything down and
;;;   returns to the frameengine as if no time had passed.
;;;
;;; During the smiley phase:
;;;   * IRQs go $FFFE -> irq_shim_fffe -> jmp($0314) -> multiplexer ISR
;;;   * PRE_ROUTINE  = $1003 (first music call per frame)
;;;   * POST_ROUTINE = smiley_post (frame logic + second music call)
;;;   * IRQ exits go to my_irq_exit (6-byte PLA/TAY/PLA/TAX/PLA/RTI stub)
;;;
;;; smiley_run and smiley_shutdown stay as no-op RTS — the frameengine
;;; will call smiley_run every frame after smiley_init returns, but by
;;; then sprites are off and there's nothing to do.
;;; =====================================================================

	.include	"moiety.smiley.i"
	.include	"t7d/memoryfunctions.i"
	.include	"t7d/vic/vicmacros.i"
	.include	"globals.i"
	.include	"LAMAlib.inc"

;;; muplex-sprites macros need SPRMUX_NO_ZP set before include so that
;;; msprite_y is placed in .code instead of .zeropage
SPRMUX_NO_ZP = 1
	.include	"LAMAlib-muplex-sprites.inc"

;;; ---------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------

NUM_SPRITES     = 20                ; 4 cols x 5 rows
FIRST_COSTUME   = $80               ; ($E000-$C000)/64 -- sprites live at $E000
                                    ; in VIC bank $C000..$FFFF

;;; Safe VIC coordinate bounds for blob origin (top-left sprite)
BLOB_X_MIN      = 24
BLOB_X_MAX_LO   = <249
BLOB_X_MAX_HI   = >249
BLOB_Y_MIN      = 50
BLOB_Y_MAX      = 162

;;; ---------------------------------------------------------------------
;;; Variables
;;; ---------------------------------------------------------------------

	.bss
saved_irq:       .res 2             ; original $FFFE/$FFFF (feirqroutine)
saved_d011:	.res	2
saved_d016:	.res	2
frame_counter:   .res 2             ; down-counter, POST_ROUTINE decrements
frame_done_flag: .res 1             ; set by POST_ROUTINE, cleared by busy-wait

blob_x_lo:       .res 1
blob_x_hi:       .res 1
blob_y:          .res 1
vel_x_lo:        .res 1
vel_x_hi:        .res 1
vel_y:           .res 1

anim_frame:      .res 1
costume_base:    .res 1
move_tick:       .res 1             ; counts down, every 2 frames move
costume_tick:    .res 1             ; counts down, every 10 frames new costume
tmp_lo:          .res 1
spr_save:        .res 1

;;; ---------------------------------------------------------------------
;;; Tables required by m_sprmultiplexer_local.inc (normally in libT7D.lib,
;;; but the local .include path bypasses the lib — so we define them here)
;;; ---------------------------------------------------------------------

	.rodata
.export _twopotentials
_twopotentials:		.byte $01,$02,$04,$08,$10,$20,$40,$80

.export _maskedtwopotentials
_maskedtwopotentials:	.byte $FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F

;;; ---------------------------------------------------------------------
;;; Tables (offsets from blob origin for each of the 20 sprites)
;;; ---------------------------------------------------------------------

	.rodata
sprite_x_off_lo:
	.byte <0, <24, <48, <72
	.byte <0, <24, <48, <72
	.byte <0, <24, <48, <72
	.byte <0, <24, <48, <72
	.byte <0, <24, <48, <72
sprite_x_off_hi:
	.byte >0, >24, >48, >72
	.byte >0, >24, >48, >72
	.byte >0, >24, >48, >72
	.byte >0, >24, >48, >72
	.byte >0, >24, >48, >72
sprite_y_off:
	.byte  0,  0,  0,  0
	.byte 21, 21, 21, 21
	.byte 42, 42, 42, 42
	.byte 63, 63, 63, 63
	.byte 84, 84, 84, 84
sprite_idx_tab:
	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19

;;; =====================================================================
;;; IRQ infrastructure (kernal-off substitutes)
;;; =====================================================================

	.code

;;; my_irq_exit: 6-byte IRQ return stub replacing $EA81/$EA31 while the
;;; Kernal ROM is swapped out. The local m_sprmultiplexer_local.inc
;;; redirects its hardcoded jmps here via IRQ_EXIT_ADDR.
my_irq_exit:
	pla
	tay
	pla
	tax
	pla
	rti

;;; irq_shim_fffe: $FFFE target during smiley phase. The upstream
;;; multiplexer uses the kernal IRQ indirection through $0314, which
;;; assumes the kernal ROM pushes A/X/Y first. We do that ourselves
;;; here before jumping via $0314.
irq_shim_fffe:
	pha
	txa
	pha
	tya
	pha
	jmp ($0314)

;;; =====================================================================
;;; POST_ROUTINE: runs once per frame after all sprites are placed
;;; =====================================================================

.proc	smiley_post
	;; second music call of the frame
	jsr $1003

	;; frame countdown
	lda frame_counter
	bne :+
	dec frame_counter+1
:	dec frame_counter

	;; wake busy-wait
	inc frame_done_flag

	;; move blob every 2 frames
	dec move_tick
	bne skip_move
	lda #2
	sta move_tick
	jsr move_blob
skip_move:

	;; switch costume every 10 frames
	dec costume_tick
	bne skip_costume
	lda #10
	sta costume_tick
	jsr next_costume
skip_costume:

	;; update all 20 sprite positions in multiplexer arrays
	jsr update_positions

	jmp my_irq_exit
.endproc

;;; =====================================================================
;;; Blob / animation logic (ported from petscii2sprites/smiley_saver.s)
;;; =====================================================================

.proc	update_positions
	ldx #NUM_SPRITES-1
loop:
	;; Y position
	lda blob_y
	clc
	adc sprite_y_off,x
	setSpriteY X, A

	;; X position (16-bit)
	stx spr_save
	txa
	tay

	lda blob_x_lo
	clc
	adc sprite_x_off_lo,y
	sta tmp_lo
	lda blob_x_hi
	adc sprite_x_off_hi,y
	tax
	lda tmp_lo
	setSpriteX Y, AX

	ldx spr_save
	dex
	bpl loop
	rts
.endproc

.proc	next_costume
	inc anim_frame
	lda anim_frame
	cmp #4
	bcc :+
	lda #0
	sta anim_frame
:	;; fall through to set_all_costumes
.endproc

.proc	set_all_costumes
	;; costume_base = FIRST_COSTUME + anim_frame * 20
	;; = FIRST_COSTUME + anim_frame*16 + anim_frame*4
	lda anim_frame
	asl
	asl
	asl
	asl                         ; *16
	sta costume_base
	lda anim_frame
	asl
	asl                         ; *4
	clc
	adc costume_base
	clc
	adc #FIRST_COSTUME
	sta costume_base

	ldx #NUM_SPRITES-1
loop:
	lda costume_base
	clc
	adc sprite_idx_tab,x
	setSpriteCostume X, A
	dex
	bpl loop
	rts
.endproc

.proc	move_blob
	;; X axis (16-bit)
	lda blob_x_lo
	clc
	adc vel_x_lo
	sta blob_x_lo
	lda blob_x_hi
	adc vel_x_hi
	sta blob_x_hi

	lda blob_x_hi
	bmi clamp_left
	bne check_right
	lda blob_x_lo
	cmp #BLOB_X_MIN
	bcs check_right
clamp_left:
	lda #BLOB_X_MIN
	sta blob_x_lo
	lda #0
	sta blob_x_hi
	jsr negate_vel_x
	jmp move_y

check_right:
	lda blob_x_hi
	cmp #BLOB_X_MAX_HI
	bcc move_y
	bne clamp_right
	lda blob_x_lo
	cmp #BLOB_X_MAX_LO
	bcc move_y
clamp_right:
	lda #BLOB_X_MAX_LO
	sta blob_x_lo
	lda #BLOB_X_MAX_HI
	sta blob_x_hi
	jsr negate_vel_x

move_y:
	lda blob_y
	clc
	adc vel_y
	sta blob_y

	cmp #BLOB_Y_MIN
	bcs check_bottom
	lda #BLOB_Y_MIN
	sta blob_y
	jsr negate_vel_y
	rts

check_bottom:
	cmp #BLOB_Y_MAX
	bcc done
	lda #BLOB_Y_MAX
	sta blob_y
	jsr negate_vel_y
done:
	rts
.endproc

.proc	negate_vel_x
	lda vel_x_lo
	eor #$FF
	clc
	adc #1
	sta vel_x_lo
	lda vel_x_hi
	eor #$FF
	adc #0
	sta vel_x_hi
	rts
.endproc

.proc	negate_vel_y
	lda vel_y
	eor #$FF
	clc
	adc #1
	sta vel_y
	rts
.endproc

;;; =====================================================================
;;; Sprite multiplexer module (local patched copy)
;;; =====================================================================

.scope	sprmux
	MAXSPRITES               = ::NUM_SPRITES
	SPRMUX_NO_ZP             = 1
	ENABLE_OVERLAY           = 0
	ENABLE_YPRIORITY         = 0
	ENABLE_GROUNDED          = 0
	ENABLE_UPDATE_ATTRIBUTES = 1
	DEBUG_RASTER_TIME        = 0
	PRE_ROUTINE              = $1003
	ENABLE_PRE_ROUTINE       = 1
	POST_ROUTINE             = ::smiley_post
	ENABLE_POST_ROUTINE      = 1
	IRQ_EXIT_ADDR            = ::my_irq_exit
	IRQ_EXIT_ADDR_DONE       = ::my_irq_exit
	.include	"m_sprmultiplexer_local.inc"
.endscope

;;; =====================================================================
;;; smiley_init: full take-over entry point
;;; Called ONCE from frameengine (channel 0) inside feirqroutine's
;;; IRQ context (I=1, all regs pushed by feirqroutine's PushRegs).
;;; Does not return until Smiley_runtime frames have elapsed.
;;; =====================================================================

.proc	smiley_init
	;; Save current $FFFE/$FFFF (= feirqroutine installed by init in main.s)
	lda $FFFE
	sta saved_irq+0
	lda $FFFF
	sta saved_irq+1

	;; Store VIC registers.
	lda	$d011
	sta	saved_d011
	lda	$d016
	sta	saved_d016
	;; Blank display: illegal mode (ECM+BMM set simultaneously) = black
	;; background, sprites still visible. Border stays as-is.
	lda #%01111111
	sta $d011
	lda	#%00001000	; 40 chars per row.
	sta	$d016

	;; Initial HW sprite pointers at $CBF8..$CBFF point to first 8 blocks
	;; at $E000. The multiplexer overwrites them each IRQ but we want
	;; sensible defaults for the 1-2 raster lines before the first
	;; multiplexer IRQ fires.
	.repeat 8,I
	lda #FIRST_COSTUME+I
	sta RNR_screen0+$3f8+I
	.endrepeat
	lda #0
	sta $d010                   ; sprite X-MSBs cleared
	SetScreenMemory RNR_screen0

	;; Blob state
	lda #<136
	sta blob_x_lo
	lda #>136
	sta blob_x_hi
	lda #50
	sta blob_y
	lda #2
	sta vel_x_lo
	lda #0
	sta vel_x_hi
	lda #1
	sta vel_y
	lda #0
	sta anim_frame

	;; Frame bookkeeping
	ldax #Smiley_runtime
	stax frame_counter
	lda #0
	sta frame_done_flag
	lda #2
	sta move_tick
	lda #10
	sta costume_tick

	;; Fill multiplexer sprite arrays
	jsr set_all_costumes
	jsr update_positions

	;; Show all 20 logical sprites, hardcoded yellow (colour 7)
	ldx #NUM_SPRITES-1
show_loop:
	setSpriteColor X, 7
	showSprite X
	dex
	bpl show_loop

	;; Tell multiplexer where the screen is ($0288 = Kernal screen-page
	;; variable, used by m_init to calculate sprite pointer addresses).
	;; With Kernal off this holds the default $04 ($0400), but our
	;; screen is at $C800 so we patch it before m_init.
	lda #>RNR_screen0           ; = $c8
	sta $0288

	;; Init multiplexer. This does sei+cli internally and sets $0314 to
	;; multiplexer_isr0. After m_init, interrupts are enabled — but we
	;; are running INSIDE feirqroutine's IRQ so a re-entrant IRQ would
	;; now fire at the multiplexer's raster line. That's fine.
	m_init sprmux

	;; Redirect $FFFE to our shim so future IRQs reach the multiplexer
	;; ISR via $FFFE -> shim -> jmp($0314)
	sei
	lda #<irq_shim_fffe
	sta $FFFE
	lda #>irq_shim_fffe
	sta $FFFF
	cli

	;; Busy-wait: each POST_ROUTINE call increments frame_done_flag and
	;; decrements frame_counter. We exit when frame_counter reaches 0.
wait_loop:
	lda frame_done_flag
	beq wait_loop
	lda #0
	sta frame_done_flag

	lda frame_counter
	ora frame_counter+1
	bne wait_loop

	;; === Shutdown ===
	sei

	;; Turn multiplexer off (sets $d015=0 and $0314 to minimum_isr,
	;; does its own sei+cli pair)
	jsr sprmux::disable_multiplexer
	sei                         ; re-disable, disable_multiplexer ended with cli

	;; Restore Kernal screen-page variable to default ($0400)
	lda #>$0400                 ; = $04
	sta $0288

	;; Restore $FFFE back to feirqroutine
	lda saved_irq+0
	sta $FFFE
	lda saved_irq+1
	sta $FFFF

	;; Reset raster IRQ line to 0 (feirqroutine's original setting)
	lda #0
	sta $d012
	lda $d011
	and #$7f                    ; clear bit 7 (raster high bit)
	sta $d011

	;; Final display: text mode, blank screen
	lda	saved_d011	    ; old mode
	and	#$7f
	sta	$d011
	lda	saved_d016
	sta	$d016
	lda #0
	sta $d015                   ; belt-and-braces: sprites off
	lda #RNR_bordercolour
	sta $d020

	;; Return to feirqroutine with IRQs still disabled —
	;; feirqroutine's PullRegs + RTI restores I from stack.
	rts
.endproc

;;; =====================================================================
;;; smiley_run: no-op. Installed on channel 2 by main.s eventtable
;;; right after smiley_init returns, called every frame until the
;;; eventtable replaces channel 2. We have nothing to do since
;;; smiley_init ran the show and cleaned up.
;;; =====================================================================
.proc	smiley_run
	rts
.endproc

;;; =====================================================================
;;; smiley_shutdown: no-op. Exported for completeness (moiety.smiley.i
;;; declares .global smiley_shutdown) but never called from the
;;; eventtable — teardown is done inline at the end of smiley_init.
;;; =====================================================================
.proc	smiley_shutdown
	rts
.endproc
