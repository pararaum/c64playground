;; For further information see http://www.antimon.org/dl/c64/code/stable.txt
; Select the video timing (processor clock cycles per raster line)
;CYCLES = 65     ; 6567R8 and above, NTSC-M
;CYCLES = 64    ; 6567R5 6A, NTSC-M
CYCLES = 63    ; 6569 (all revisions), PAL-B

cinv = $314
cnmi = $318
raster = 52     ; start of raster interrupt
m = $fb         ; zero page variable

  .word $0801
  * = $801
basic:
  .word eobas   ; link to next line
  .word 1995    ; line number
  .byte $9E     ; SYS token
  .asc "2061"

eobas
  .byte 0,0,0   ; end of BASIC program
  
start:
  jmp install
  jmp deinstall

install:        ; install the raster routine
  jsr restore   ; Disable the Restore key (disable NMI interrupts)
checkirq:
  lda cinv      ; check the original IRQ vector
  ldx cinv+1    ; (to avoid multiple installation)
  cmp #<irq1
  bne irqinit
  cpx #>irq1
  beq skipinit
irqinit:
  sei
  sta oldirq    ; store the old IRQ vector
  stx oldirq+1
  lda #<irq1
  ldx #>irq1
  sta cinv      ; set the new interrupt vector
  stx cinv+1
skipinit:
  lda #$1b
  sta $d011     ; set the raster interrupt location
  lda #raster
  sta $d012
  ldx #$e
  clc
  adc #3
  tay
  lda #0
  sta m
loop78:
  lda m
  sta $d000,x   ; set the sprite X
  adc #24
  sta m
  tya
  sta $d001,x   ; and Y coordinates
  dex
  dex
  bpl loop78
  lda #$7f
  sta $dc0d     ; disable timer interrupts
  sta $dd0d
  ldx #1
  stx $d01a     ; enable raster interrupt
  lda $dc0d     ; acknowledge CIA interrupts
  lsr $d019     ; and video interrupts
  ldy #$ff
  sty $d015     ; turn on all sprites
  cli
  rts

deinstall:
  sei           ; disable interrupts
  lda #$1b
  sta $d011     ; restore text screen mode
  lda #$81
  sta $dc0d     ; enable Timer A interrupts on CIA 1
  lda #0
  sta $d01a     ; disable video interrupts
  lda oldirq
  sta cinv      ; restore old IRQ vector
  lda oldirq+1
  sta cinv+1
  bit $dd0d     ; re-enable NMI interrupts
  cli
  rts

; Auxiliary raster interrupt (for syncronization)
irq1:
; irq (event)   ; > 7 + at least 2 cycles of last instruction (9 to 16 total)
; pha           ; 3
; txa           ; 2
; pha           ; 3
; tya           ; 2
; pha           ; 3
; tsx           ; 2
; lda $0104,x   ; 4
; and #xx       ; 2
; beq           ; 3
; jmp ($314)    ; 5
                ; ---
                ; 38 to 45 cycles delay at this stage
  lda #<irq2
  sta cinv
  lda #>irq2
  sta cinv+1
  nop           ; waste at least 12 cycles
  nop           ; (up to 64 cycles delay allowed here)
  nop
  nop
  nop
  nop
  inc $d012     ; At this stage, $d012 has already been incremented by one.
  lda #1
  sta $d019     ; acknowledge the first raster interrupt
  cli           ; enable interrupts (the second interrupt can now occur)
  ldy #9
  dey
  bne *-1       ; delay
  nop           ; The second interrupt will occur while executing these
  nop           ; two-cycle instructions.
  nop
  nop
  nop
oldirq = * + 1  ; Placeholder for self-modifying code
  jmp *         ; Return to the original interrupt

; Main raster interrupt
irq2:
; irq (event)   ; 7 + 2 or 3 cycles of last instruction (9 or 10 total)
; pha           ; 3
; txa           ; 2
; pha           ; 3
; tya           ; 2
; pha           ; 3
; tsx           ; 2
; lda $0104,x   ; 4
; and #xx       ; 2
; beq           ; 3
; jmp (cinv)    ; 5
                ; ---
                ; 38 or 39 cycles delay at this stage
  lda #<irq1
  sta cinv
  lda #>irq1
  sta cinv+1
  ldx $d012
  nop
#if CYCLES - 63
#if CYCLES - 64
  nop           ; 6567R8, 65 cycles/line
  bit $24
#else
  nop           ; 6567R56A, 64 cycles/line
  nop
#endif
#else
  bit $24       ; 6569, 63 cycles/line
#endif
  cpx $d012     ; The comparison cycle is executed CYCLES or CYCLES+1 cycles
                ; after the interrupt has occurred.
  beq *+2       ; Delay by one cycle if $d012 hadn't changed.
                ; Now exactly CYCLES+3 cycles have passed since the interrupt.
  dex
  dex
  stx $d012     ; restore original raster interrupt position
  ldx #1
  stx $d019     ; acknowledge the raster interrupt
  ldx #2
  dex
  bne *-1
  nop
  nop
  lda #20       ; set the amount of raster lines-1 for the loop
  sta m
  ldx #$c8
irqloop:
  ldy #2
  dey
  bne *-1       ; delay
  dec $d016     ; narrow the screen (exact timing required)
;3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
;ssssssssss                                               ||ssssss Phi-2 VIC-II
;==========xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||XXX====== Phi-2 6510
;          ^ now we are here
  stx $d016     ; expand the screen
#if CYCLES - 63
#if CYCLES - 64
  bit $24       ; 6567R8
#else
  nop           ; 6567R56A
#endif
#else
  nop           ; 6569
#endif
  dec m
  bmi endirq
  clc
  lda $d011
  sbc $d012
  and #7
  bne irqloop   ; This instruction takes 4 cycles instead of 3,
                ; because the page boundary is crossed.
badline:
  dec m
  nop
  nop
  nop
  nop
  dec $d016
;3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
;ssssssssss    cccccccccccccccccccccccccccccccccccccccc   ||ssssss Phi-2 VIC-II
;==========xXXX========================================||***====== Phi-2 6510
;          ^ we are here
  stx $d016
;3s4s5s6s7srrrrrgggggggggggggggggggggggggggggggggggggggg--||0s1s2s Phi-1 VIC-II
;ssssssssss                                               ||ssssss Phi-2 VIC-II
;==========xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx||XXX====== Phi-2 6510
;          ^ ^^- we are here (6569)
;          | \- or here (6567R56A)
;          \- or here (6567R8)
  ldy #2
  dey
  bne *-1
  nop
  nop
#if CYCLES - 63
#if CYCLES - 64
  nop           ; 6567R8, 65 cycles/line
  nop
  nop
#else
  bit $24       ; 6567R56A, 64 cycles/line
#endif
#else
  nop           ; 6569, 63 cycles/line
#endif
  dec m
  bpl irqloop   ; This is a 4-cycle branch (page boundary crossed)
endirq:
  jmp $ea81     ; return to the auxiliary raster interrupt

restore:        ; disable the Restore key
  lda cnmi
  ldy cnmi+1
  pha
  lda #<nmi     ; Set the NMI vector
  sta cnmi
  lda #>nmi
  sta cnmi+1
  ldx #$81
  stx $dd0d     ; Enable CIA 2 Timer A interrupt
  ldx #0
  stx $dd05
  inx
  stx $dd04     ; Prepare Timer A to count from 1 to 0.
  ldx #$dd
  stx $dd0e     ; Cause an interrupt.
nmi = * + 1
  lda #$40      ; RTI placeholder
  pla
  sta cnmi
  sty cnmi+1    ; restore original NMI vector (although it won't be used)
  rts


