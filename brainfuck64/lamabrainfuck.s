; ********************************************************
; Brainfuck Interpreter for C64/128
; by Wil 2021
;
; Usage: put start adress of BF program in $02/03
;        
;
; ********************************************************

.include "LAMAlib.inc"

.MACPACK longbranch

prg_ptr  =$2
data_ptr =$4

DATA_SIZE =$5000	;must be multiples of $100
DEFAULT_DATA_AREA=$5000

execute_prg:
	lda data_ptr
	if ne
	  pokew data_ptr,DEFAULT_DATA_AREA
	endif

	; clear data area
	ldx #>DATA_SIZE
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
	ldy #00
	lda (prg_ptr),y
	if eq
	  rts
	endif
	inc16 prg_ptr
	switch A
	case '+'
	  lda (data_ptr),y
	  ;sec   ;C==1 because of matched case CMP
	  adc #0
	  sta (data_ptr),y
	  jmp interpret
	case '-'
	  lda (data_ptr),y
	  ;sec   ;C==1 because of matched case CMP
	  sbc #1
	  sta (data_ptr),y
	  jmp interpret

	case '>'
	  inc16 data_ptr
	  beq interpret ;unconditional

	case '<'
	  dec16 data_ptr
	  jmp interpret

	case '.'
	  lda (data_ptr),y
output_char:
	  cmp cr_char
	  if eq
	    lda #$0d
	  endif
	  jsr CHROUT
	  jmp interpret

	case ','
	  do
	    jsr GETIN
	    tax
	  loop while eq
	  cmp #$0d
	  if eq
cr_char=*+1
	    lda #$0a
	  endif
	  
	  ldy #00
	  sta (data_ptr),y
	  beq output_char	;unconditional

	case '['
	  lda (data_ptr),y
	  bne interpret		;data byte was not 0, we don't jump
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
