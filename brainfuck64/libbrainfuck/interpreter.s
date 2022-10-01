; ********************************************************
; Brainfuck Interpreter for C64/128
; by Wil 2021
;
; ********************************************************
	.SETCPU "6502X"

	.include "LAMAlib.inc"
	.MACPACK longbranch


	.import	__BRAINFUCKWORK_START__
	.import	__BRAINFUCKWORK_SIZE__
	.importzp	ptr1,ptr2

	shadow_d020 = $7f6
	shadow_d021 = $7f7

	use_illegal_opcodes = 1

	.export	interpret_brainfuck
	
	.zeropage
prg_ptr:	.res	2
data_ptr:	.res	2
mem_config:	.res	1	;intended memory configuration

	.code
interpret_brainfuck:
	sta	prg_ptr
	stx	prg_ptr+1
	pokew	data_ptr,__BRAINFUCKWORK_START__
	lda	$1
	sta	mem_config	;remember current memory configuration

	; clear data area
	ldx #>__BRAINFUCKWORK_SIZE__
	lda data_ptr+1
	pha
	ldy #0
	tya
	do
      	  do
      	    sta (data_ptr),y
      	    iny
      	  loop until eq
      	  inc data_ptr+1
      	  dex
	loop until eq
	pla
	sta data_ptr+1

	;start interpreting

interpret:
	bit	$91		; Get status of RUN/STOP.
	bmi	@running	; Key is not pressed, continue the loop.
	sec
	rts
@running:
	ldy #00
	lda (prg_ptr),y
	if eq
	  clc
	  rts			; Return to caller.
	endif
	inc16 prg_ptr
	switch A
	case '+'
.if use_illegal_opcodes=0
	  lda (data_ptr),y
	  ;sec   ;C==1 because of matched case CMP
	  adc #0
	  sta (data_ptr),y
.else
	  isc (data_ptr),y
.endif
	  jmp interpret
	case '-'
.if use_illegal_opcodes=0
	  lda (data_ptr),y
	  ;sec   ;C==1 because of matched case CMP
	  sbc #1
	  sta (data_ptr),y
.else
	  dcp (data_ptr),y
.endif
	  jmp interpret
	case '>'
	  inc16 data_ptr
	  bne interpret ;unconditional

	case '<'
	  dec16 data_ptr
	  jmp interpret

	case '.'
	  lda (data_ptr),y
output_char:
	  jsr ascii_to_petscii
	  ldx #$36
	  stx $1
	  jsr CHROUT
	  ldx mem_config
	  stx $1
jmp_interpret:
	  jmp interpret

	case ','
	  ldx #$36
	  stx $1
	  turn_on_cursor
	  do
	    jsr GETIN
	    tax
	  loop while eq
	  pha
	  turn_off_cursor
	  pla
	  ldx mem_config
	  stx $1
	  jsr petscii_to_ascii
 
	  ldy #00
	  sta (data_ptr),y
	  beq jmp_interpret	;unconditional

	case '['
	  lda (data_ptr),y
	  bne jmp_interpret		;data byte was not 0, we don't jump
	  ;jmp forward to next matching ]
	  ldx #1

search_loop_forward:
	  lda (prg_ptr),y
	  switch A
	  case ']'
	    dex
            beq exit_loop_forward
	  case '['
	    inx
	    break
          endswitch
	  iny
	  bne search_loop_forward
	  inc prg_ptr+1
	  bne search_loop_forward
exit_loop_forward:
	  tya
	  ;sec	we know that C==1 because we had a matched CMP
	  adc prg_ptr	;+1, b/c carry is set
	  sta prg_ptr
	  if cs
            inc prg_ptr+1
          endif
	  jmp interpret

	case ']'
	  lda (data_ptr),y
	  jeq interpret		;data byte was 0, we don't jump	  
	  ldy #$FE
	  dec prg_ptr+1
	  ;jmp backward to next matching ]
	  ldx #1

search_loop_backward:
	  lda (prg_ptr),y
	  switch A
	  case '['
	    dex
            beq exit_loop_backward
	  case ']'
	    inx
	    break
          endswitch
	  dey
          cpy #$ff
	  bne search_loop_backward
	  dec prg_ptr+1
	  bne search_loop_backward
exit_loop_backward:
	  tya
	  ;sec	we know that C==1 because we had a matched CMP
	  adc prg_ptr	;+1, b/c carry is set
	  sta prg_ptr
	  if cs
            inc prg_ptr+1
          endif
	  ;fallthrough
	endswitch
	jmp interpret



; convert PETSCII value in A to ASCII code
.proc petscii_to_ascii
	cmp #97
	if cs
	  cmp #123
	  if cc
	    eor #$20
	    rts
	  endif
	endif
	cmp #65
	if cs
	  cmp #91
	  if cc
	    eor #$20
	    rts
	  endif
	endif
	cmp #$0d
	if eq
	  lda #$0a
	endif
	rts
.endproc

; convert ASCII value in A to PETSCII code
.proc ascii_to_petscii
	cmp #97
	if cs
	  cmp #123
	  if cc
	    eor #$20
	    rts
	  endif
	endif
	cmp #65
	if cs
	  cmp #91
	  if cc
	    eor #$20
	    rts
	  endif
	endif
	cmp #$0a
	if eq
	  lda #$0d
	endif
	; intended side effects of F5 and F7 key
        cmp #135	;F5 key? -> dark background
        if eq
          ldx #00
          stx shadow_d021
        endif
        cmp #136	;F7 key? -> next background color
        if eq
          inc shadow_d021
        endif
	rts
.endproc
