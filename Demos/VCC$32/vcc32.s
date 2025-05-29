.include "LAMAlib.inc"

length_part_mirror=5000	;in ms, shuold be long enough to let the scroller run through
part_rocknscroll=$4400

;SID Music player
music_init = $1000
music_play = music_init+3
music_current_instrument = $133f
;Instruments:
;  Heartbeat $13
;  Werbel    $24
;  Finish    $28

SCREEN=$400

rasterline1=0
rasterline2=81
rasterline3=153
rasterline4=50+8*23
vicmemset1=%00010101  ;ROM charset, screen at $400
vicmemset4=%00011001  ;charset at $2000, screen at $400

scrolltext = $1710
WATER_HEIGHT=64
PHASE_OFFSET=0
SCROLLTEXTCOLOR=14

daumenkino=$2800

effects=eff0+1

QUICKSTART=0	;if >0 the intro is skipped
CANDLEANIMATION=0
RASTERBARS=0


;.FEATURE string_escapes

;-----------------------------------------------
; Initialization
;-----------------------------------------------

	clrscr
	poke 53280,0
	poke 53281,0
	delay_ms 1000

	; init music
	lda #00
	jsr music_init

	sei
	lda #$7f
	sta $dc0d	; disable timer interrupts
	sta $dd0d
	lda $dc0d	; acknowledge CIA interrupts

	;set irq line
	lda #$7f
	and $D011
	sta $D011 	; Clear most significant bit in VIC's raster register
	lda #50
	sta $D012 	; Set the raster line number where interrupt should occur 

	lda #01
	sta $D01A 	; enable raster interrupt

	lda #<isr1	;set ISR addr
	sta $314
	lda #>isr1
	sta $315	
	cli

;-----------------------------------------------
; Demo code
;-----------------------------------------------
.if QUICKSTART
	jmp quickstart
.endif

	lda #$13
:	cmp music_current_instrument
	bne :-

	;lda #$13
:	cmp music_current_instrument
	bne :-

	lda #<string1
	ldx #>string1
	ldy #20-(string2-string1)/2
	clc
	jsr fadeprint

	delay_ms 200

	lda #$13
:	cmp music_current_instrument
	bne :-

	lda #<string2
	ldx #>string2
	ldy #20-(string3-string2)/2
	clc
	jsr fadeprint
	delay_ms 200

	lda #$13
:	cmp music_current_instrument
	bne :-

	lda #<string3
	ldx #>string3
	ldy #20-(string4-string3)/2
	clc
	jsr fadeprint

	lda #$13
:	cmp music_current_instrument
	bne :-

	lda #<string4
	ldx #>string4
	ldy #20-(string5-string4)/2
	sec
	jsr fadeprint

	lda #$24
:	cmp music_current_instrument
	bne :-

	lda #$24
:	cmp music_current_instrument
	bne :-

	jsr daumenkino

quickstart:
	lda #<petsciiimg0
	ldx #>petsciiimg0
	jsr displayPETSCII
	jsr initscroller
	inc effects

;BEGIN DEBUG
;lda #01
;ldx #00
;lp1: sta $d020
;stx $d020
;jmp lp1

;END DEBUG
	do
	  lda $dc00
	  and $dc01
	  and #16
	  until eq
	  lda scrollSessions
	loop while eq

;.repeat length_part_mirror/65536
;	delay_ms 65000
;.endrep
;	delay_ms (length_part_mirror & $ffff)
	poke $1000,$60
	jmp part_rocknscroll

string1:  .asciiz "welcome to vcc $32"
string2:  .asciiz "that's fifty in hex!"
string3:  .asciiz "hack, code, share, play"
string4:  .asciiz "from carinthia with love"
string5:

;-----------------------------------------------
; Fadeprint
; Print a string and fade the colors away
;-----------------------------------------------
.export fadeprint

fadeprint: 
	sta sma+1	;stringaddr
	stx sma+3
	sty smy+1	;column

	lda #00
	bcc skp
	lda #colors2-colors
skp:
	sta smc+1

fade_loop:
smc:	ldx #00
	lda colors,x
	sta 646
	inc smc+1

	ldx #12
smy:	ldy #5
	clc
	jsr $FFF0	;place cursor

sma:	lda #00
        ldy #00
        jsr STROUT	;print string
	delay_ms 200

	lda 646
	bne fade_loop

	rts


colors:	 .byte 1,7,15,12,11,0
colors2: .byte 1,1,7,7,15,12,12,11,0

;-----------------------------------------------
; Scroller code
; Scroll a text in double size
;-----------------------------------------------

.export initscroller,scroll

nextpattern=828 ;nobody needs the cassette buffer these days
scrollcnt=nextpattern+16
carrysave=nextpattern+17
charbase=$2000
charbasesrc=$D800
line=23
zpp1=$22

initscroller:
	;clear first 2*40 chars (our scrolling area)
	ldx #214+1
	lda #00
	sta carrysave
	sta scrollcnt
loopc:	sta charbase-1,x
	sta charbase+213,x
	sta charbase+427,x	
	dex
	bne loopc

	;place 2*40 chars
	ldx #39
loopp:	sta SCREEN+line*40,x
	clc
	adc #01
	sta SCREEN+line*40+40,x
	adc #01
	pha
	lda #SCROLLTEXTCOLOR
	sta $D800+line*40,x
	sta $D800+line*40+40,x
	pla
	dex
	bpl loopp
	rts

resetscroller:
	inc scrollSessions
		
	;reset scrolltext
	lda #<scrolltext
	sta getnextchar+1
	lda #>scrolltext
	sta getnextchar+2

	;fallthrough!

getnextchar: lda scrolltext
	beq resetscroller
	inc16 getnextchar+1

	sta zpp1
	lda #0
	sta zpp1+1

	;multiply zpp1 by 8
	asl zpp1
	rol zpp1+1
	asl zpp1
	rol zpp1+1
	asl zpp1
	rol zpp1+1
	; add char base
	lda zpp1+1
	adc #>charbasesrc
	sta zpp1+1

	;copy the char
	ldy #7
	ldx #15

	;switch banking to make charset visible
	lda #%00110011
	sta 1

copylp:	lda (zpp1),y
	sta nextpattern,x
	dex
	sta nextpattern,x
	dex
	dey
	bpl copylp

	;back to banking with I/O area visible
	lda #%00110111
	sta 1

	;reset scroll flag and count
	;lda #$10
	lda #$8
	sta scrollcnt

	;fallthrough!

scroll: 
	dec scrollcnt
	bmi getnextchar

	ldx #0		;upcount loop to move behind rasterline
scrolllp:
	;lda nextpattern,x
	;rol
	;lda #01
	;bit scrollcnt
	;bne skprl
	rol nextpattern,x
skprl:

; the following code does not need unrolling, but is very slow. 
; Updating two rows of characters takes over 30000 cycles
;

;	lda #<charbase
;	sta addr+1
;	lda #>charbase
;	sta addr+2
;	ldy #40
;addr:	rol charbase,x
;	rol carrysave ;because the next addition screws the carry flag
;	clc	;the previous operation also cleared the carry
;	lda addr+1
;	adc #16
;	sta addr+1
;	bcc cntd1
;	inc addr+2
;cntd1:	ror carrysave
;	dey
;	bpl addr

	;ROL the line two times (because of speed)
	php
	jsr shiftline
	plp
	jsr shiftline

	inx
	cpx #16
	beq exitlp
	jmp scrolllp
exitlp:
	rts

;this code is just unrolled 40 ROL commands running much faster
shiftline:
.repeat 40, I
	rol charbase+I*16,x
.endrep
	rts

scrollSessions:
	.byte 0

;-----------------------------------------------
; The IRQ routines
;-----------------------------------------------

.export isr1

;-----------------------------------------------
;isr1 at rasterline 0

isr1:	asl $D019	;acknowlegde interrupt
	lda #vicmemset1	
	sta $D018	;restore char memory
	jsr music_play	

eff0:	lda #00
	beq exisr

	lda #rasterline2	;next raster
	sta $D012
	lda #<isr2
	sta $314
	lda #>isr2
	sta $315

exisr:	jmp $EA81	;exit via oringinal IQ routine

;-----------------------------------------------
;isr2 just before the big VCC

isr2:
	asl $D019	;acknowlegde interrupt

.if RASTERBARS
	lda #121	;next raster
	sta $D012	
	lda #<isr25
	sta $314
	lda #>isr25
	sta $315
.else
	lda #rasterline3	;next raster
	sta $D012	
	lda #<isr3
	sta $314
	lda #>isr3
	sta $315
.endif


	bne smx	;unconditional jump since zero flag is never set

resetsx:	
	sta smx+1    ;A is zero when we come here
smx:	ldx #00
	lda scrollvals,x
	beq resetsx
	sta $D016
	inc smx+1
	jmp $EA81


.if RASTERBARS
;-----------------------------------------------
;isr25 before th rasterbar in line 122

isr25:
	; delay_ms for rasterline 122
	lda #121
	ldx #12
	ldy #0
	jsr delaysr
	stx $D020

	; delay_ms for rasterline 130
	lda #129
	jsr delaysr
	sty $D020
	ldx #15
	lda #137
	jsr delaysr
	stx $D020
	lda #145
	jsr delaysr
	sty $D020	;border is now back in black
	lda $D016
	ora #$08	;enable 40 chars width again
	sta $D016

	asl $D019	;acknowlegde interrupt
	lda #rasterline3	;next raster
	sta $D012	

	lda #<isr3
	sta $314
	lda #>isr3
	sta $315

	jmp $EA81	;exit IRQ routine


delaysr:
	cmp $D012
	bcs delaysr
	sec
	lda #09
delaylp:sbc #1
	bne delaylp
	rts

.endif

;-----------------------------------------------
;isr3 after the big VCC
isr3:	asl $D019	;acknowlegde interrupt
	lda #rasterline4	;next raster
	sta $D012	


	lda #<isr4
	sta $314
	lda #>isr4
	sta $315

	;water effect
	
	sec
	lda smx+1
.if PHASE_OFFSET>0
	sbc #PHASE_OFFSET
	bcs no_wrap_around
	adc #40
no_wrap_around:	
.endif
	sta smx2+1

	ldy #WATER_HEIGHT
	sty watercount
smx2:	ldx #00
loopwt:	lda scrollvals,x
	bne skipresetx
	ldx #00
	lda scrollvals,x
skipresetx:

	ldy $D012
:	cpy $D012
	beq :-

	sta $D016
	inx
	dec watercount 
	bpl loopwt

	inc smx2+1
	ldx smx2+1
	lda scrollvals,x
	bne skipresetx2
	lda #00
	sta smx2+1
skipresetx2:
	LDA ($A1,X)	;kill 6 cycles
	LDA ($A1,X)	;kill 6 cycles

	lda #200
	sta $D016

	jmp $EA81	;exit IRQ routine


scrollvals:
	.byte 196,196,197,197,198,198,198,199,199,199,199,199,199,199,198,198,198,197,197,196
	.byte 196,195,194,194,193,193,193,192,192,192,192,192,192,192,193,193,193,194,194,195,0
watercount: .byte 00

;-----------------------------------------------
;isr4 just before the scroller

isr4:	asl $D019	;acknowlegde interrupt
	lda #rasterline1	;next raster
	sta $D012	

	lda #<isr1
	sta $314
	lda #>isr1
	sta $315

	lda #vicmemset4
	sta $D018	;char memory $2000

.if CANDLEANIMATION
	;animate candle
	dec candlecount
	bne nocandleanim
	lda #4	;animation speed for candle flame
	sta candlecount
cycle:	ldx #0
	lda candlechars,x
	bne updatecandle
	sta cycle+1
	beq cycle
updatecandle:
	sta SCREEN+38
	inc cycle+1
nocandleanim:
.endif

	cli	;allow to be interrupted
	jsr scroll
	jmp $EA81	;exit IRQ routine

.if CANDLEANIMATION
candlechars:
	.byte 123,98,252,97,236,97,0
candlecount: .byte 20
.endif

;-----------------------------------------------
; PETSCII Decode and display
; to use, load address of compressed PETSCII img in A/X
; and call displayPETSCII:
;  lda #<petsciiimg5
;  ldx #>petsciiimg5
;  jsr displayPETSCII
;-----------------------------------------------

.include "displayPETSCII.s"

;-----------------------------------------------
; PETSCII Image
;-----------------------------------------------

.export petsciiimg0
.include "vcc32.petscii.s"





